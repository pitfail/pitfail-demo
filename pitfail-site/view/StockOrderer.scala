
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
import lib.formats._
import intform._

import stockdata._
import model.derivatives._
import model.Schema.User
import scalaz.Scalaz._

import lib.formats._

abstract class StockOrder
case class NoOrder() extends StockOrder
case class BuyShares(quote: Quote, volume: BigDecimal) extends StockOrder
case class AddToDerivative(quote: Quote, volume: BigDecimal) extends StockOrder

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
        lazy val form: Form[BigDecimal] = Form(
            identity[BigDecimal],
            (
                volumeField
            ),
            <div class="block" id="search-buy">
                <h2>Choose Volume</h2>
                <p>Enter the amount you would like to purchase in dollars. This will be
                converted to shares and will be added to your portfolio.</p>

                <p class="price-input">
                    ${volumeField.main & <input id="search-quantity"/>} {volumeField.errors}
                    <span class="error">{submitBuy.errors}{submitAdd.errors}</span>
                </p>

                <div class="buttons">
                    {submitBuy.main & <input/>} 
                    {submitAdd.main & <input/>} 
                    {submitCancel.main & <input/>} 
                </div>
            </div>
        )

        lazy val volumeField = NumberField("1.00")
    
        lazy val submitBuy = Submit(form, "Buy Shares") { volume =>
            buyStock(quote, volume)
        }
        
        lazy val submitAdd = Submit(form, "Add to Derivative") { v =>
            addStockToDerivative(quote, v)
        }

        lazy val submitCancel = Submit(form, "Cancel") { v =>
            notifyAndRefresh(NoOrder())
        }
        
        form.render
    }

    private def buyStock(quote: Quote, volume: BigDecimal): JsCmd = {
        import control.LoginManager._
        import model.Schema._

        try {
            currentUser.mainPortfolio.buyStock(quote.stock.symbol, volume)
            currentQuote = None
            notifyAndRefresh(BuyShares(quote, volume))
        } catch {
            case NegativeVolume => throw BadInput(
                "You must buy more than $0.00 of a stock"
            )
            case NotEnoughCash(have, need) => throw BadInput(
                "You need at least %s you only have %s" format (need.$, have.$)
            )
            case NotLoggedIn =>
                throw BadInput("You must be logged in to buy stock")
        }
    }

    private def addStockToDerivative(quote: Quote, volume: BigDecimal) = {
        currentQuote = None
        notifyAndRefresh(AddToDerivative(quote, volume))
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

