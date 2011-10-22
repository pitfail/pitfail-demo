
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
import matteform._

import stockdata._
import model.derivatives._
import model.Schema.User
import scalaz.Scalaz._

import lib.formats._

class QueryStockOld extends RefreshableSnippet with Loggable
{
    private val stockDatabase: StockDatabase = new YahooStockDatabase(new HttpQueryService("GET"))
    private var currentQuote: Option[Quote] = None
    private var order: SortedMap[String, (Quote, BigDecimal)] = TreeMap()

    def render(p: RefreshPoint)(in: NodeSeq) = (
           in
        |> queryForm.render(p) _
        |> quoteForm.render(p) _
        |> renderQuote(p) _
        |> renderList(p) _
    )

    object queryForm extends Form[Stock](
        AggregateField((symbol: String) => Stock(symbol toUpperCase),
                StringField("query", "")
            :^: KNil
        ),
        formID = Some("search-query")
    )
    {
        override def act(stock: Stock) {
            try {
                currentQuote = Some(stockDatabase.getQuotes(Iterable(stock)).head)
            } catch {
                case _: NoSuchStockException => throw BadInput(
                    "There is no stock with symbol " + stock.symbol + "."
                )

                case _: Exception => throw BadInput(
                    "An unknown error has occurred."
                )
            }
        }
    }

    object quoteForm extends Form[BigDecimal](
        NumberField("quantity", "1"),
        formID = Some("search-buy")
    )
    {
        override def act(quantity: BigDecimal) {}
    }

    def renderQuote(p: RefreshPoint)(in: NodeSeq): NodeSeq = {
        (currentQuote match {
            case Some(quote) => (
                  ".quote-company *"    #> quote.company
                & ".quote-ticker *"     #> quote.stock.symbol
                & ".quote-price *"      #> quote.price.$
                & ".quote-change *"     #> (tryGetNumber(quote.info.percentChange) + "%")
                & ".quote-open *"       #> tryGetPrice(quote.info.openPrice)
                & ".quote-low *"        #> tryGetPrice(quote.info.lowPrice)
                & ".quote-high *"       #> tryGetPrice(quote.info.highPrice)
                & ".quote-dividend *"   #> tryGetPrice(quote.info.dividendShare)
                & ".quote-graph [src]"  #> "http://ichart.finance.yahoo.com/instrument/1.0/%s/chart;range=1d/image;size=239x110"
                                             .format(quote.stock.symbol toLowerCase)

                & "#search-button-buy"  #> (button => SHtml.ajaxSubmit((button\"@value").text, () => {
                    quoteForm.processField match {
                        case Some(volume) => buyStock(quote, volume)
                        case None         => throw BadInput("An unknown error has occured.")
                    }
                    p.refreshCommand
                }))
                & "#search-button-add"  #> (button => SHtml.ajaxSubmit((button\"@value").text, () => {
                    quoteForm.processField match {
                        case Some(volume) => addStockToDerivative(quote, volume)
                        case None         => throw BadInput("An unknown error has occured.")
                    }
                    p.refreshCommand
                }))
            )

            case None => (
                 "#search-quote" #> Nil
                & "#search-buy"  #> Nil
                //same
            )
        })(in)
    }

    def renderList(p: RefreshPoint)(in: NodeSeq): NodeSeq = {
        (if (order isEmpty) {
            "#search-derivative" #> Nil
            //same
        } else {
            ( "#search-list-row" #> (order map (_ match {
                case (_, (quote, quantity)) => {
                    // TODO: What if the user doesn't have enough money?
                    val shares = BigDecimal((quantity / quote.price).toInt)
                    ( ".search-list-ticker *"   #> quote.stock.symbol
                    & ".search-list-company *"  #> quote.company
                    & ".search-list-price *"    #> quote.price.$
                    & ".search-list-shares *"   #> shares.toString
                    & ".search-list-subtotal *" #> ((shares * quote.price).$))
                }
              }))
            & ".search-list-total *" #> (getTotalPrice(order.values).toString))
        })(in)
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

    private def tryGetNumber(a: Option[BigDecimal]): String = {
        a match {
            case Some(number) => number.toString
            case None         => "n/a"
        }
    }

    private def tryGetPrice(a: Option[BigDecimal]): String = {
        a match {
            case Some(number) => number.$
            case None         => "n/a"
        }
    }

    private def getTotalPrice(quotes: Iterable[(Quote, BigDecimal)]): BigDecimal = {
        (quotes map (_ match {
            case (quote, quantity) => BigDecimal((quantity / quote.price).toInt) * quote.price
        })).reduceLeft[BigDecimal](_ + _)
    }
}

