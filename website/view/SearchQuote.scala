
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

import org.joda.time.Duration

import formats._

class SearchQuote extends Page with Loggable
{
    private val stockDatabase: StockDatabase = new CachedStockDatabase(
        new YahooStockDatabase(new HttpQueryService("GET")),
        // TODO: This timeout should be moved to a configuration file.
        new Duration(1000 * 60 * 5)
    )
    private var currentQuote: Option[Quote] = None;
    private var listeners: List[Option[Quote] => JsCmd] = Nil;

    private val refreshable = Refreshable(
        <div id="search" class="container">
            {queryForm.render}
            {quoteBlock}
        </div>
    )

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

    def changeQuote(stock: Stock): JsCmd = {
        currentQuote = Some(stockDatabase.getQuotes(Iterable(stock)).head)
        notifyAndRefresh(currentQuote)
    }

    def clearQuote: JsCmd = {
        currentQuote = None
        notifyAndRefresh(currentQuote)
    }

    override def render = refreshable.render
    
    /*
     * Search Form
     */
    lazy val queryForm: Form[Stock] = Form(
        (sym: String) => Stock(sym toUpperCase),
        (
            tickerField
        ),
        <div id="search-query">
            {tickerField.main & <input id="search-query-field"/>}
            {tickerField.errors}
            {submitStock.main}
            {submitStock.errors}
        </div>
    )
    
    lazy val tickerField = StringField("")

    lazy val submitStock = Submit(queryForm, "Search") { stock =>
        try {
            changeQuote(stock)
        } catch {
            case _: NoSuchStockException => throw BadInput(
                "There is no stock with symbol " + stock.symbol + "."
            )
        }
    }

    /*
     * Quote
     */
    def quoteBlock =
        currentQuote match {
            case Some(quote) => quoteBlockPresent(quote)
            case None        => Nil
        }

    def quoteBlockPresent(quote: Quote) = 
        <div id="search-quote" class="quote block">
            <h2>{quote.stock.symbol} - {quote.company}</h2>
            <h3>{quote.price.$} {quote.info.percentChange.%()}</h3>
            <dl> {
                import quote.info._
                (<dt>Open</dt>    <dd class="quote-open">{openPrice.$}</dd>
                <dt>Low</dt>      <dd class="quote-low">{lowPrice.$}</dd>
                <dt>High</dt>     <dd class="quote-high">{highPrice.$}</dd>
                <dt>Dividend</dt> <dd class="quote-dividend">{dividendShare.$}</dd>)
            } </dl>
            {quote |> quoteGraph}
        </div>

    def quoteGraph(quote: Quote) =
        <img
            class="quote-graph"
            alt="stock price over time"
            src={graphURL(quote)}
        />
        
    def graphURL(quote: Quote) =
        "http://ichart.finance.yahoo.com/instrument/1.0/%s/chart;range=1d/image;size=239x110"
         .format(quote.stock.symbol toLowerCase)
}
