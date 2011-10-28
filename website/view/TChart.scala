
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import control.LoginManager
import formats._
import intform._

import model._
import model.Schema._
import LoginManager.{currentLogin}
import stockdata._
import org.joda.time.Duration

class TChart(
    user:       User,
    modifiable: Boolean
)
    extends Renderable
    with Loggable
{
    // TODO: This should be a singleton object to take full advantage of
    //       caching.
    private val stockDatabase: StockDatabase = new CachedStockDatabase(
        new YahooStockDatabase(new HttpQueryService("GET")),
        // TODO: This timeout should be moved to a configuration file.
        new Duration(1000 * 60 * 5)
    )

    def render = trans {
        val port = user.mainPortfolio
        
        val myStockAssets           = port.myStockAssets
        val myCashAmount            = port.cash
        val myDerivativeAssets      = port.myDerivativeAssets
        val myDerivativeLiabilities = port.myDerivativeLiabilities
        
        lazy val result =
            <div class="block">
                <h2>Portfolio</h2>
                <table id="portfolio" class="block container portfolio">
                    <col class="tchart-left1"/>
                    <col class="tchart-left2"/>
                    <col class="tchart-right1"/>
                    <col class="tchart-right2"/>
                    <tr class="tchart-top">
                        <th colspan="2" class="tchart-top">Assets</th>
                        <th colspan="2" class="tchart-top">Liabilities</th>
                    </tr>
                    <tr>
                        <td colspan="2" class="tchart-half">{assetsTable}</td>
                        <td colspan="2" class="tchart-half">{liabilitiesTable}</td>
                    </tr>
                    <tr class="tchart-section tchart-total">
                        <td>Total:</td>
                        <td class="tchart-dollars">{total.$}</td>
                        <td>Total:</td>
                        <td class="tchart-dollars">{liabilitiesTotal.$}</td>
                    </tr>
                </table>
            </div>
        
        lazy val assetsTable =
            <table class="tchart assets">
                <tr class="tchart-section">
                    <th>Cash</th>
                    <td/>
                    <td/>
                    <td class="tchart-dollars">{myCashAmount.$}</td>
                </tr>

                <tr class="tchart-section">
                    <th colspan="2">Stocks</th>
                    <th colspan="1"/>
                    <th class="tchart-dollars">{stocksTotal.$}</th>
                </tr>
                {stocks}

                <tr class="tchart-section">
                    <th colspan="2">Derivatives</th>
                    <th/>
                    <th class="tchart-dollars">**{derivsTotal.$}</th>
                </tr>
                {derivativeAssets}
            </table>
                
        lazy val stocksTotal = (
                     myStockAssets
                 map stockDollars _
                 map (_ getOrElse Dollars(0))
             )
            .foldLeft(Dollars("0"))(_ + _)
        
        lazy val derivsTotal = (myDerivativeAssets
                map (_.derivative) map (_.spotValue))
            .foldLeft(Dollars("0"))(_ + _)
            
        lazy val total = stocksTotal + myCashAmount + derivsTotal
            
        lazy val stocks = {
            if (myStockAssets isEmpty)
                <tr><td colspan="3">none</td></tr>
            else
                myStockAssets map { (asset) =>
                    val sellButton =
                        if (modifiable)
                            <td class="buttons">
                                {snippet.SellThisStock(asset.ticker)}
                            </td>
                        else Nil
            
                    <tr>
                        <td>{asset.ticker}</td>
                        {sellButton}
                        <td class="tchart-price">({mehPrice(asset)}/sh)</td>
                        <td class="tchart-dollars">{mehDollars(asset)}</td>
                    </tr>
                }
        }
        
        lazy val derivativeAssets =
            if (myDerivativeAssets isEmpty)
                <tr><td colspan="3">none</td></tr>
            else
                myDerivativeAssets map { (asset) =>
                    val deriv = asset.derivative
                    val liab = asset.peer
                    
                    <tr class="deriv-row deriv-header">
                        <td>Secs:</td>
                        <td>{deriv.securities toHumanString}</td>
                        <td class="buttons">{
                            if (modifiable && asset.derivative.early)
                                execDerivative(asset)
                            else
                                Nil
                        }</td>
                        <td class="tchart-dollars">**{deriv.spotValue.$}</td>
                    </tr>
                    <tr class="deriv-row">
                        <td>From:</td>
                        <td>{UserLink(liab.owner.owner.username)}</td>
                    </tr>
                    <tr class="deriv-row">
                        <td>On:</td>
                        <td>{deriv.exec toNearbyString}</td>
                    </tr>
                    <tr class="deriv-row">
                        <td>If:</td>
                        <td>{deriv.condition toHumanString}</td>
                    </tr>
                }
        
        lazy val liabilitiesTable =
            <table class="tchart liabilities">
                <tr class="tchart-section">
                    <th colspan="2">Derivatives</th>
                    <th/>
                    <th class="tchart-dollars">**{derivativeLiabilitiesTotal.$}</th>
                </tr>
                {derivativeLiabilities}
            </table>

        lazy val valuedDerivativeLiabilities = myDerivativeLiabilities map { lia =>
            (lia, lia.derivative.spotValue * lia.remaining)
        }
        
        lazy val derivativeLiabilities =
            if (myDerivativeLiabilities isEmpty)
                <tr><td colspan="3">none</td></tr>
            else
                valuedDerivativeLiabilities map { case (liability, dollars) =>
                    val deriv = liability.derivative
                    
                    <tr class="deriv-row deriv-header">
                        <td>Secs:</td>
                        <td>{deriv.securities toHumanString}</td>
                        <td> </td>
                        <td class="tchart-dollars">**{dollars.$}</td>
                    </tr>
                    <tr class="deriv-row">
                        <td>On:</td>
                        <td>{deriv.exec toNearbyString}</td>
                    </tr>
                    <tr class="deriv-row">
                        <td>If:</td>
                        <td>{deriv.condition toHumanString}</td>
                    </tr>
                }
        
        lazy val derivativeLiabilitiesTotal = (valuedDerivativeLiabilities
            map (_._2)).foldLeft(Dollars(0))(_ + _)
        
        lazy val liabilitiesTotal = derivativeLiabilitiesTotal
        
        result
    }
    
    def execDerivative(da: DerivativeAsset) = FormSubmit.rendered("Exercise") {
        da.refetch() map {execute _} match {
            case Some(_) =>
                comet.Portfolio ! comet.Refresh
                Noop
            case None => throw BadInput("No longer exists")
        }
    }
    
    def execute(da: DerivativeAsset) {
        try {
            da.executeManually()
        }
        catch {
            case NotExecutable => throw BadInput("Not executable")
        }
    }
    
    def mehDollars(asset: StockAsset): String =
        stockDollars(asset) map (_.$) getOrElse "???"
    
    def stockDollars(asset: StockAsset): Option[Dollars] =
        stockPrice(asset) map (_ * asset.shares)
    
    def mehPrice(asset: StockAsset): String =
        stockPrice(asset) map (_.$) getOrElse "???"
    
    def stockPrice(asset: StockAsset): Option[Price] = {
        try {
            val stock  = Stock(asset.ticker)
            val quote  = stockDatabase.getQuotes(Seq(stock)).head
            Some(quote.price)
        }
        catch {
            case _: NoSuchStockException => None
            case _: DatabaseException    => None
        }
    }
}

object TChart {
    def apply(u: User, m: Boolean) = new TChart(u, m)
}

