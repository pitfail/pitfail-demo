
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
    private var currentQuote: Option[Quote] = None;
    private var listeners: List[StockOrder => JsCmd] = Nil;

    private val refreshable = Refreshable(
        currentQuote match {
            case Some(quote) => purchaseForm(quote)
            case None        => Nil
        }
    )
    
    def purchaseForm(quote: Quote): NodeSeq = {
        lazy val form: Form[Dollars] = Form(
            identity[Dollars],
            (
                volumeField: Field[Dollars]
            ),
            <div class="block" id="search-buy">
                <h2>Choose Volume</h2>
                <p>Enter the amount you would like to purchase in dollars. This will be
                converted to shares and will be added to your portfolio.</p>

                <p class="price-input">
                    ${volumeField.main & <input id="search-quantity" class="blank"/>}
                    <span class="error">{submitBuy.errors}{submitAdd.errors}</span>
                </p>

                <div class="buttons">
                    {submitBuy.main & <input/>} 
                    {submitAdd.main & <input/>} 
                    {submitCancel.main & <input/>} 
                </div>
            </div>
        )

        lazy val volumeField = new DollarsField("1000") with FieldErrorRender
    
        lazy val submitBuy = Submit(form, "Buy Shares") { volume =>
            import control.LoginManager._

            try {
                buyStock(quote, volume)
            } catch {
                case NegativeVolume => throw BadFieldInput(volumeField,
                    "You must buy more than $0.00 of a stock"
                )

                case NotEnoughCash(have, need) => throw BadFieldInput(volumeField,
                    "You need at least %s you only have %s" format (need.$, have.$)
                )

                /* TODO: change to redirect to the login and then redirect back */
                case NotLoggedIn =>
                    throw BadInput("You must be logged in to buy stock")
            }
        }
        
        lazy val submitAdd = Submit(form, "Add to Derivative") { v =>
            addStockToDerivative(quote, v /-/ quote.price)
        }

        lazy val submitCancel = Submit(form, "Cancel") { v =>
            notifyAndRefresh(NoOrder())
        }
        
        form.render
    }

    private def buyStock(quote: Quote, dollars: Dollars): JsCmd = {
        import control.LoginManager._
        
        this.logger.info("Buying %s of %s" format(dollars, quote))

        val shares = dollars /-/ quote.price

        if (shares > Shares(0)) {
            currentUser.mainPortfolio.userBuyStock(quote.stock.symbol, shares)
            currentQuote = None

            comet.Portfolio ! comet.Refresh
            comet.News      ! comet.Refresh
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

