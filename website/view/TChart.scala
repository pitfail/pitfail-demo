
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

import LoginManager.{currentLogin}
import stockdata._
import org.joda.time.Duration

import model._
import model.schema._

object tChart extends Loggable {
//

def apply(port: Portfolio, currentUser: Option[User], modifiable: Boolean) = {
//
    
var showHidden = false

lazy val refreshable = Refreshable(doRender)
def render = refreshable.render
    
def doRender: NodeSeq = {
    val myStockAssets           = port.myStockAssets
    val myCashAmount            = port.cash
    val myMarginAmount          = port.margin
    val myDerivativeAssets      = port.myDerivativeAssets
    val myDerivativeLiabilities = port.myDerivativeLiabilities
    
    lazy val result =
        <div class="block">
            <h2>Portfolio {sharing}</h2>
            {table}
            {hiddenControls}
        </div>
        
    lazy val sharing = currentUser map { currentUser =>
        val others = port.owners filter (o => !(o~~currentUser))
        this.logger.info("Shared with " + others)
        if (others.length == 0) Nil
        else {
            val links = others map (snippet.UserLink(_))
            val list = links reduceLeft { (a, b) =>
                a ++ <span>, </span> ++ b
            }
            <span class="sharing">(Shared with {list})</span>
        }
    } getOrElse <span/>
    
    lazy val table =
        <table id="portfolio" class="block container portfolio">
            <col class="tchart-left1"/>
            <col class="tchart-left2"/>
            <col class="tchart-right1"/>
            <col class="tchart-right2"/>
            <tr class="tchart-top">
                <td colspan="2" class="tchart-top">Assets</td>
                <td colspan="2" class="tchart-top">Liabilities</td>
            </tr>
            <tr>
                <td colspan="2" class="tchart-half">{assetsTable}</td>
                <td colspan="2" class="tchart-half">{liabilitiesTable}</td>
            </tr>
            <tr class="tchart-section tchart-total tchart">
                <td>Total:</td>
                <td class="tchart-dollars">{total.$}</td>
                <td>Total:</td>
                <td class="tchart-dollars">{liabilitiesTotal.$}</td>
            </tr>
            <tr class="tchart">
                <td class="tchart-section tchart-total">
                    Equity:
                </td>
                <td class="tchart-section tchart-total tchart-dollars">
                {(total - liabilitiesTotal).$}
                </td>
            </tr>
        </table>

    lazy val assetsTable =
        <table class="tchart assets">
            <tr class="tchart-section">
                <td>Cash</td>
                <td/>
                <td/>
                <td class="tchart-dollars">{myCashAmount.$}</td>
            </tr>
            <tr class="tchart-section">
                <td>Margin</td>
                <td/>
                <td/>
                <td class="tchart-dollars">{myMarginAmount.$}</td>
            </tr>

            <tr class="tchart-section">
                <td colspan="2">Stocks</td>
                <td colspan="1"/>
                <td class="tchart-dollars">{stocksTotal.$}</td>
            </tr>
            {stocks}

            <tr class="tchart-section">
                <td colspan="2">Derivatives</td>
                <td/>
                <td class="tchart-dollars">**{derivsTotal.$}</td>
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
            <tr><td colspan="4">none</td></tr>
        else
            myStockAssets map { (asset) =>
                <tr>
                    <td class="tchart-ticker">{asset.ticker}</td>
                    <td class="tchart-price">({asset.shares.###() + " @ " + mehPrice(asset)})</td>
                    <td class="buttons">
                    {
                    if (modifiable)
                        snippet.SellThisStock(asset.ticker)
                    else
                        Nil
                    }
                    </td>
                    <td class="tchart-dollars">{mehDollars(asset)}</td>
                </tr>
            }
    }
    
    lazy val derivativeAssets =
        if (myDerivativeAssets isEmpty)
            <tr class="deriv-row deriv-header"><td colspan="3">none</td></tr>
        else
            myDerivativeAssets flatMap renderDerivativeAsset _
    
    def renderDerivativeAsset(asset: DerivativeAsset) =
        if (asset.hidden && !showHidden) Nil
        else {
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
                <td colspan="3">{PortfolioLink(liab.owner)}</td>
            </tr>
            <tr class="deriv-row">
                <td>On:</td>
                <td colspan="3">{deriv.exec toNearbyString}</td>
            </tr>
            <tr class="deriv-row">
                <td>If:</td>
                <td colspan="3">{deriv.condition toHumanString}</td>
            </tr>
        }
    
    lazy val liabilitiesTable =
        <table class="tchart liabilities">
            <tr class="tchart-section">
                <td colspan="2">Derivatives</td>
                <td/>
                <td class="tchart-dollars">**{derivativeLiabilitiesTotal.$}</td>
            </tr>
            {derivativeLiabilities}
        </table>

    lazy val valuedDerivativeLiabilities = myDerivativeLiabilities map { lia =>
        (lia, lia.derivative.spotValue * lia.remaining)
    }
    
    lazy val derivativeLiabilities =
        if (myDerivativeLiabilities isEmpty)
            <tr><td colspan="4">none</td></tr>
        else
            valuedDerivativeLiabilities flatMap (renderDerivativeLiability _).tupled
    
    def renderDerivativeLiability(liability: DerivativeLiability, dollars: Dollars) =
        if (liability.hidden && !showHidden) Nil
        else {
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
    try {
        da.userExecuteManually()
    }
    catch { case NoSuchDerivativeAsset =>
        throw BadInput("No longer exists")
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
        Some(model.Stocks lastTradePrice asset.ticker)
    }
    catch {
        case e: NoSuchStockException =>
            logger.error("Ugh", e)
            None
            
        case e: DatabaseException    => None
            logger.error("Ugh", e)
            None
    }
}

def hiddenControls =
    if (   (port.myDerivativeAssets exists (_.hidden))
        || (port.myDerivativeLiabilities exists (_.hidden)) )
    {
        lazy val showLink = FormSubmit.rendered("Show Hidden") {
            showHidden = true
            refreshable.refresh
        }
        
        lazy val hideLink = FormSubmit.rendered("Hide Hidden") {
            showHidden = false
            refreshable.refresh
        }
        
        if (showHidden)
            <p>show hidden <span class="button">{hideLink}</span></p>
        else
            <p><span class="button">{showLink}</span> hide hidden</p>
    }
    else Nil
    
render
}

//
}

