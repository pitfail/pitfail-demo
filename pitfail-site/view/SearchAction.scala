
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
                </p>

                <div class="buttons">
                {submitBuy.main & <input id="search-button-buy"/>} {submitBuy.errors}
                {submitBuy.main & <input id="search-button-add"/>} {submitAdd.errors}
                </div>
            </div>
        )

        lazy val volumeField = NumberField("1.00")
    
        lazy val submitBuy = Submit(form, "Buy") { v =>
            //buyStock(quote, v)
            form.refresh()
        }
        
        lazy val submitAdd = Submit(form, "Add") { v =>
            //addStockToDerivative(quote, v)
            form.refresh()
        }
        
        form.render
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
