
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
import model.derivatives._
import model.Schema.User
import scalaz.Scalaz._
import formats._

class SearchDerivativeBuilder extends Page with Loggable
{
    private var stocks: SortedMap[String, (Quote, BigDecimal)] = TreeMap()
    private var listeners: List[Option[Quote] => JsCmd] = Nil;

    implicit def toDollars(price: Option[BigDecimal]) = new {
        def $: String = {
            (price map (_.$)) getOrElse "n/a" 
        }
    }

    implicit def toPercent(percent: Option[BigDecimal]) = new {
        def %(): String = {
            (percent map (_.toString + "%")) getOrElse "n/a"
        }
    }

    implicit def round(decimal: BigDecimal) = new {
        def round(decimals: Int, mode: RoundingMode): BigDecimal = {
            val precision = decimal.precision - decimal.scale + decimals
            val context = new MathContext(precision, mode)
            decimal.round(context)
        }

        def floor: BigDecimal =
            round(0, RoundingMode.FLOOR)
    }

    private val refreshable = Refreshable(
        if (stocks.isEmpty)
            Nil
        else
            <table class="block" id="search-list">
                <thead>
                    <tr>
                        <th class="search-list-ticker">Ticker</th>
                        <th class="search-list-company">Company</th>
                        <th class="search-list-price">Price</th>
                        <th class="search-list-shares">Shares</th>
                        <th class="search-list-subtotal">Subtotal</th>
                        <th class="search-list-buttons"/>
                    </tr>
                </thead>
                <tbody>
                    {stocks map { case (symbol, (quote, volume)) => formatStockRow(quote, volume) }}
                </tbody>
                <tfoot>
                    <tr>
                        <th colspan="4">Total</th>
                        <td class="search-list-total">{getTotalVolume.$}</td>
                        <td class="search-list-buttons"/>
                    </tr>
                </tfoot>
            </table>
    )

    private def formatStockRow(quote: Quote, requestedVolume: BigDecimal) = {
        val shares = (requestedVolume / quote.price).floor
        val actualVolume = shares * quote.price
        <tr>
            <td class="search-list-ticker">{quote.stock.symbol}</td>
            <td class="search-list-company">{quote.company}</td>
            <td class="search-list-price">{quote.price.$}</td>
            <td class="search-list-shares">{shares}</td>
            <td class="search-list-subtotal">{actualVolume.$}</td>
            <td class="search-list-buttons">
                <input type="submit" value="Remove"/>
            </td>
        </tr>
    }

    private def notify(quote: Option[Quote]): JsCmd =
        (listeners map { (callback) => callback(quote) }).foldLeft(Noop)(_ & _)

    private def notifyAndRefresh(quote: Option[Quote]): JsCmd = {
        notify(quote) & refreshable.refresh
    }

    /*
     * Public API
     */
    def listen(callback: Option[Quote] => JsCmd) {
        listeners ::= callback
    }

    def addQuote(quote: Quote, volume: BigDecimal): JsCmd = {
        stocks = stocks + ((quote.stock.symbol, (quote, volume)))
        refreshable.refresh
    }

    def getTotalVolume: BigDecimal = {
        (stocks map {
            // TODO: This should be a floor.
            case (_, (quote, requestedVolume)) => {
                (requestedVolume / quote.price).floor * quote.price
            }
        }).fold[BigDecimal](0)(_ + _)
    }

    override def render = refreshable.render
}
