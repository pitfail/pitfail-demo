
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
            <div id="portfolio" class="block container portfolio">
                <h2>Portfolio</h2>
                <table class="tchart assets">
                    <thead>
                        <tr><th colspan="4"><h3>Assets</h3></th></tr>
                    </thead>
                    <tbody>
                        <tr class="tchart-section">
                            <th>Cash</th>
                            <td/>
                            <td class="tchart-dollars">{myCashAmount.$}</td>
                            <td/>
                        </tr>

                        <tr class="tchart-section">
                            <th colspan="2">Stocks</th>
                            <th class="tchart-dollars">{stocksTotal.$}</th>
                            <th colspan="1"/>
                        </tr>
                        {stocks}

                        <tr class="tchart-section">
                            <th colspan="2">Derivatives</th>
                            <th class="tchart-dollars">**{derivsTotal.$}</th>
                            <th/>
                        </tr>
                        {derivativeAssets}
                        
                        <tr class="tchart-section tchart-total">
                            <th colspan="2">Total</th>
                            <th class="tchart-dollars">{total.$}</th>
                            <th colspan="1"/>
                        </tr>
                    </tbody>
                </table>
                
                <table class="tchart liabilities">
                    <thead>
                        <tr><th colspan="3"><h3>Liabilities</h3></th></tr>
                    </thead>
                    <tbody>
                        <tr><th colspan="3">Derivatives</th></tr>
                        {derivativeLiabilities}
                    </tbody>
                </table>
                <div style="clear:both;"/>
            </div>

        lazy val stocksTotal = (myStockAssets map stockDollars)
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
                        <td class="tchart-price">({stockPrice(asset).$}/sh)</td>
                        <td class="tchart-dollars">{stockDollars(asset).$}</td>
                        {sellButton}
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
                        <td class="tchart-dollars">**{deriv.spotValue.$}</td>
                        <td class="buttons">{
                            if (modifiable && asset.derivative.early)
                                execDerivative(asset)
                            else
                                Nil
                        }</td>
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
        
        lazy val derivativeLiabilities =
            if (myDerivativeLiabilities isEmpty)
                <tr><td colspan="3">none</td></tr>
            else
                myDerivativeLiabilities map { (liability) =>
                    <tr>
                        <td colspan="2">{liability.derivative toHumanString}</td>
                        <td>{
                            if (liability.remaining < Scale("1"))
                                liability.remaining.%()
                            else
                                Nil
                        }</td>
                    </tr>
                }
        
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
    
    def stockDollars(asset: StockAsset): Dollars = {
        val shares = asset.shares
        val stock  = Stock(asset.ticker)
        val quote  = stockDatabase.getQuotes(Seq(stock)).head
        shares * quote.price
    }
    
    def stockPrice(asset: StockAsset): Price = {
        val stock  = Stock(asset.ticker)
        val quote  = stockDatabase.getQuotes(Seq(stock)).head
        quote.price
    }
}

object TChart {
    def apply(u: User, m: Boolean) = new TChart(u, m)
}

