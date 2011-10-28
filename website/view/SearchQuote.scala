
package code

package snippet

import java.math.{MathContext,RoundingMode}

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http.{StringField => _, _}
import js._
import JsCmds._
import JE._
import Helpers._

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.math.{BigDecimal}
import intform._

import stockdata._
import model.StockPriceSource
import model.Schema.User
import model.derivatives._
import scalaz.Scalaz._

import org.joda.time.Duration

import formats._

class SearchQuote extends Page with Loggable
{
    private var currentQuote: Option[Quote] = None;
    private var listeners: List[Option[Quote] => JsCmd] = Nil;

    private val refreshable = Refreshable(
        <div id="search" class="container">
            {queryForm.render}
            {quoteBlock}
        </div>
    )

    private def notify(quote: Option[Quote], cmd: JsCmd = Noop): JsCmd =
        (listeners map { (callback) => callback(quote) }).foldLeft(Noop)(_ & _)

    private def notifyAndRefresh(quote: Option[Quote], cmd: JsCmd = Noop): JsCmd = {
        // This order is important because cmd may apply to the new contents.
        notify(quote, Noop) & refreshable.refresh & cmd
    }

    /*
     * Public API
     */
    def listen(callback: Option[Quote] => JsCmd) {
        listeners ::= callback
    }

    def changeQuote(stock: Stock): JsCmd = {
        currentQuote = Some(StockPriceSource.getQuotes(Iterable(stock)).head)
        notifyAndRefresh(currentQuote, Focus("search-quantity"))
    }

    def clearQuote: JsCmd = {
        currentQuote = None
        notifyAndRefresh(currentQuote, Focus("search-query-field"))
    }

    override def render = refreshable.render
    
    /*
     * Search Form
     */
    lazy val queryForm: Form[Stock] = Form(
        (sym: String) => Stock(sym toUpperCase),
        (
            tickerField: Field[String]
        ),
        <div id="search-query">
            <div id="search-query-field-hack">
                {tickerField.main & <input id="search-query-field"/>}
            </div>
            {submitStock.main & <input id="search-query-button"/>}
            {submitStock.errors}
        </div> ++
        <p>{tickerField.errors}</p>
    )
    
    lazy val tickerField = new StringField("")

    lazy val submitStock = Submit(queryForm, "Go") { stock =>
        try {
            changeQuote(stock)
        } catch {
            case _: NoSuchStockException =>
                logger.info("Failed to find " + stock)
                
                throw BadFieldInput(
                    tickerField,
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
            case None        => quoteBlockAbsent
        }

    def quoteBlockPresent(quote: Quote) = 
        <div id="search-quote" class="quote block">
            <h2>{quote.stock.symbol} - {quote.company}</h2>
            <h3>
                <span class="quote-price">{quote.price.$}</span> -
                <span class="quote-change">{quote.info.percentChange.%()}</span>
            </h3>
            <dl> {
                import quote.info._
                (<dt>Open</dt>    <dd class="quote-open">{openPrice.$}</dd>
                <dt>Low</dt>      <dd class="quote-low">{lowPrice.$}</dd>
                <dt>High</dt>     <dd class="quote-high">{highPrice.$}</dd>
                <dt>Dividend</dt> <dd class="quote-dividend">{dividendShare.$}</dd>)
            } </dl>
            {quote |> quoteGraph}
        </div>

    def quoteBlockAbsent =
        <div id="search-instructions" class="block">
            <ol>
                <li>Enter a ticker symbol into the search field above.</li>
                <li>Choose an amount of stock to buy or add to a derivative.</li>
                <li>Login and manage your portfolio!</li>
            </ol>
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
