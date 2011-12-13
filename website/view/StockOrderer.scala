
// Written by: Michael Koval

package code
package snippet

import java.math.{MathContext,RoundingMode}

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.math.{BigDecimal}
import intform._

import stockdata._
import scalaz.Scalaz._

import formats._

import model._
import model.schema._

abstract class StockOrder
case class NoOrder() extends StockOrder
case class BuyShares(quote: Quote, dollars: Dollars) extends StockOrder
case class AddToDerivative(quote: Quote, shares: Shares) extends StockOrder

class StockOrderer extends Page with Loggable
{
    import StockOrderer._
    
    private var currentQuote: Option[Quote] = None;
    private var listeners: List[StockOrder => JsCmd] = Nil;

    private val refreshable = Refreshable(
        currentQuote match {
            case Some(quote) => purchaseForm(quote)
            case None        => Nil
        }
    )
    
    def purchaseForm(quote: Quote): NodeSeq = {
        lazy val form: Form[Order] = Form(
            Order,
            (
                volumeField: Field[Dollars],
                limitField: Field[Option[Price]]
            ),
            <div class="block" id="search-buy">
                <h2>Choose Volume</h2>
                <p>Enter the amount you would like to purchase in dollars.</p>

                <div>
                    <div style="float:left;">
                        <p class="price-input">
                            ${volumeField.main & <input id="search-quantity" class="blank"/>}
                            <span class="error">{submitBuy.errors}{submitAdd.errors}</span>
                        </p>
                    </div>
                    <div style="float:left;">
                        {availableSellers(quote.symbol)}
                    </div>
                    <div style="clear:both;"><p> </p></div>
                </div>

                {limitField.main}
                
                <div class="buttons">
                    {submitBuy.main & <input/>} 
                    {submitAdd.main & <input/>} 
                    {submitCancel.main & <input/>} 
                </div>
            </div>
        )
    
        lazy val limitField = LimitField()
    
        lazy val volumeField = new DollarsField("1000") with FieldErrorRender
    
        lazy val submitBuy = Submit(form, "Place Order") { case Order(dollars, limit) =>
            import control.LoginManager._

            try {
                limit match {
                    case None        => buyStock(quote, dollars)
                    case Some(price) => makeLimitOrder(quote, dollars, price)
                }
            } catch {
                case NegativeVolume => throw BadFieldInput(volumeField,
                    "You must buy more than $0.00 of a stock"
                )

                case NotEnoughCash(have, need) => throw BadFieldInput(volumeField,
                    "You need at least %s you only have %s" format (need.$, have.$)
                )
                
                case NoBidders => throw BadFieldInput(volumeField,
                    "There are not enough traders to satisfy that order"
                )

                /* TODO: change to redirect to the login and then redirect back */
                case NotLoggedIn =>
                    throw BadInput("You must be logged in to buy stock")
            }
        }
        
        lazy val submitAdd = Submit(form, "Add to Derivative") { case Order(v, _) =>
            addStockToDerivative(quote, v /-/ quote.price)
        }

        lazy val submitCancel = Submit(form, "Cancel") { case Order(v, _) =>
            notifyAndRefresh(NoOrder())
        }
        
        form.render
    }

    private def buyStock(quote: Quote, dollars: Dollars): JsCmd = {
        import control.LoginManager._
        import control.PortfolioSwitcher._
        
        val shares = dollars /-/ quote.price

        if (shares > Shares(0)) {
            currentPortfolio.userBuyStock(quote.stock.symbol, shares)
            currentQuote = None

            notifyAndRefresh(BuyShares(quote, dollars))
        } else {
            throw BadInput("You must buy a minimum of one share.")
        }
    }
    
    private def makeLimitOrder(quote: Quote, dollars: Dollars, limit: Price): JsCmd = {
        import control.LoginManager._
        import control.PortfolioSwitcher._
        
        val shares = dollars /-/ quote.price

        if (shares > Shares(0)) {
            currentPortfolio.userMakeBuyLimitOrder(quote.stock.symbol, shares, limit)
            currentQuote = None

            notifyAndRefresh(BuyShares(quote, dollars))
        } else {
            throw BadInput("You must buy a minimum of one share.")
        }
        
    }

    private def addStockToDerivative(quote: Quote, shares: Shares) = {
        currentQuote = None
        notifyAndRefresh(AddToDerivative(quote, shares))
    }

    private def notify(action: StockOrder): JsCmd =
        (listeners map { (callback) => callback(action) }).foldLeft(Noop)(_ & _)

    private def notifyAndRefresh(action: StockOrder): JsCmd = {
        notify(action) & refreshable.refresh
    }

    /*
     * Public API
     */
    def listen(callback: StockOrder => JsCmd) {
        listeners ::= callback
    }

    def changeQuote(quote: Quote): JsCmd = {
        currentQuote = Some(quote)
        refreshable.refresh
    }

    def clearQuote: JsCmd = {
        currentQuote = None
        refreshable.refresh
    }

    override def render = refreshable.render
}
object StockOrderer {
    case class Order(dollars: Dollars, limit: Option[Price])
}


