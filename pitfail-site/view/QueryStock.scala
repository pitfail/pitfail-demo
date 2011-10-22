
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

class QueryStock extends Page with Loggable
{
    private val stockDatabase: StockDatabase = new YahooStockDatabase(new HttpQueryService("GET"))
    private var currentQuote: Option[Quote] = None
    private var order: SortedMap[String, (Quote, BigDecimal)] = TreeMap()

    class page extends Renderable {
        def render: NodeSeq =
            <div id="search" class="container">
                {queryForm.render}
                {quoteBlock}
                {currentQuote map quoteForm _ getOrElse Nil}
            </div>
    }
    val page = new page with Refreshable
    
    def render = page.render
    
    // -----------------------------------------------------------------
    // queryForm
        
    lazy val queryForm: Form[Stock] = Form(
        (sym: String) => Stock(sym toUpperCase),
        (
            -StringField(""),
            -submitStock
        ))(F( (tick,sub) =>
            <div id="search-query">
                {tick.main & <input id="search-query-field"/>}
                {tick.errors}
                {sub.main}
                {sub.errors}
            </div>
        ))
    
    lazy val submitStock = Submit(queryForm, "Search") { stock =>
        try {
            currentQuote = Some(stockDatabase.getQuotes(Iterable(stock)).head)
            logger.info("Setting quote to " + currentQuote)
            page.refresh
        } catch {
            case _: NoSuchStockException => throw BadInput(
                "There is no stock with symbol " + stock.symbol + "."
            )
        }
    }
    
    // -----------------------------------------------------------------
    // quoteForm
        
    def quoteForm(quote: Quote) = {
        lazy val form: Form[BigDecimal] = Form(identity[BigDecimal], (
            -NumberField("1"),
            
            -Submit(form, "Buy") { v =>
                buyStock(quote, v)
                page.refresh()
            },
            -Submit(form, "Add") { v =>
                addStockToDerivative(quote, v)
                page.refresh()
            }
        ))(F( (quant,buy,add) =>
            <div class="block" id="search-buy">
                <h2>Choose Volume</h2>
                <p>Enter the amount you would like to purchase in dollars. This will be
                converted to shares and will be added to your portfolio.</p>

                <p class="price-input">
                ${quant.main & <input id="search-quantity"/>} {quant.errors}
                </p>

                <div class="buttons">
                {buy.main & <input id="search-button-buy"/>} {buy.errors}
                {add.main & <input id="search-button-add"/>} {add.errors}
                </div>
            </div>
        ))
        
        form.render
    }

    // -----------------------------------------------------------------
    // quoteBlock
        
    def quoteBlock =
        currentQuote match {
            case Some(quote) => quoteBlockPresent(quote)
            case None        => Nil
        }
    
    def quoteBlockPresent(quote: Quote) =
        <div id="search-quote" class="quote block">
            <h2>{quote.stock.symbol} - {quote.company}</h2>
            <h3>{quote.price.$} {quote |> percentChange}</h3>
            <dl> {
                import quote.info._
                (<dt>Open</dt>    <dd class="quote-open">{openPrice |> tryGetPrice}</dd>
                <dt>Low</dt>      <dd class="quote-low">{lowPrice |> tryGetPrice}</dd>
                <dt>High</dt>     <dd class="quote-high">{highPrice |> tryGetPrice}</dd>
                <dt>Dividend</dt> <dd class="quote-dividend">{dividendShare |> tryGetPrice}</dd>)
            } </dl>
            {quote |> quoteGraph}
        </div>
    
    def percentChange(quote: Quote) =
        tryGetNumber(quote.info.percentChange) + "%"
    
    def quoteGraph(quote: Quote) =
        <img
            class="quote-graph"
            alt="stock price over time"
            src={graphURL(quote)}
        />
        
    def graphURL(quote: Quote) =
        "http://ichart.finance.yahoo.com/instrument/1.0/%s/chart;range=1d/image;size=239x110"
         .format(quote.stock.symbol toLowerCase)
         
    // -----------------------------------------------------------------
    // derivativeBlock

    def orderList = <p>Coming soon..... :):):)</p>
    
    // -----------------------------------------------------------------
    // various callbacks
    
    private def buyStock(quote: Quote, volume: BigDecimal) = {
        import control.LoginManager._
        import model.Schema._

        try {
            currentUser.mainPortfolio.buyStock(quote.stock.symbol, volume)
            currentQuote = None
            
            comet.Portfolio ! comet.Refresh
            comet.News      ! comet.Refresh
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
    }

    private def tryGetNumber(a: Option[BigDecimal]) =
        a map (_.###()) getOrElse "n/a"
    
    private def tryGetPrice(a: Option[BigDecimal]) =
        a map (_.$) getOrElse "n/a"
    
    private def getTotalPrice(quotes: Iterable[(Quote, BigDecimal)]): BigDecimal = {
        (quotes map (_ match {
            case (quote, quantity) => BigDecimal((quantity / quote.price).toInt) * quote.price
        })).foldLeft[BigDecimal](0)(_ + _)
    }
}

