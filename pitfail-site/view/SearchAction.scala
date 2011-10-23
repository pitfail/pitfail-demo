
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

class SearchAction extends Page with Loggable
{
    private var currentQuote: Option[Quote] = None;
    private var listeners: List[(Option[Quote], BigDecimal) => JsCmd] = Nil;

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
                {submitBuy.main & <input id="search-button-buy"/>} 
                {submitAdd.main & <input id="search-button-add"/>} 
                </div>
            </div>
        )

        lazy val volumeField = NumberField("1.00")
    
        lazy val submitBuy = Submit(form, "Buy Shares") { volume =>
            buyStock(quote, volume)
            form.refresh
        }
        
        lazy val submitAdd = Submit(form, "Add to Derivative") { v =>
            //addStockToDerivative(quote, v)
            form.refresh
        }
        
        form.render
    }

    private def buyStock(quote: Quote, volume: BigDecimal) = {
        import control.LoginManager._
        import model.Schema._

        try {
            currentUser.mainPortfolio.buyStock(quote.stock.symbol, volume)
            currentQuote = None
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
        /*
        order = (order get quote.stock.symbol) match {
            // Add more shares to an existing stock.
            case Some((oldQuote, oldVolume)) => {
                order + ((quote.stock.symbol, (quote, oldVolume + volume)))
            }

            // Add a new stock.
            case None => {
                order + ((quote.stock.symbol, (quote, volume)))
            }
        }
        currentQuote = None
        */
    }

    private def notify(quote: Option[Quote], volume: BigDecimal): JsCmd =
        (listeners map { (callback) => callback(quote, volume) }).foldLeft(Noop)(_ & _)

    private def notifyAndRefresh(quote: Option[Quote], volume: BigDecimal): JsCmd = {
        notify(quote, volume) & refreshable.refresh
    }

    /*
     * Public API
     */
    def listen(callback: (Option[Quote], BigDecimal) => JsCmd) {
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
