
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
import view.UserField

import stockdata._
import model.derivatives._
import model.Schema.User
import scalaz.Scalaz._

import lib.formats._

class QueryStock extends RefreshableSnippet with Loggable
{
    private val stockDatabase: StockDatabase = new YahooStockDatabase(new HttpQueryService("GET"))
    private var currentQuote: Option[Quote] = None
    private var order: SortedMap[String, (Quote, BigDecimal)] = TreeMap()

    def render(p: RefreshPoint)(in: NodeSeq) = (
           in
        |> queryForm.render(p) _
        |> quoteForm.render(p) _
        |> renderQuote _
        |> renderList _
    )

    object queryForm extends Form[Stock](
        AggregateField(Stock,
                StringField("query", "")
            :^: KNil
        ),
        formID = Some("search-query")
    )
    {
        override def act(stock: Stock) {
            // TODO: Handle errors.
            try {
                currentQuote = Some(stockDatabase.getQuotes(Iterable(stock)).head)
            } catch {
                case _: NoSuchStockException => {
                    print("ERROR: No Such Stock") }
            }
        }
    }

    object quoteForm extends Form[BigDecimal](
        NumberField("quantity", "1"),
        formID = Some("search-buy")
    )
    {
        override def act(quantity: BigDecimal) {
            currentQuote match {
                // User entered a value into the query field.
                case Some(quote) => {
                    order = (order get quote.stock.symbol) match {
                        // Add more shares to an existing stock.
                        case Some((oldQuote, oldQuantity)) => {
                            order + ((quote.stock.symbol, (quote, oldQuantity + quantity)))
                        }

                        // Add a new stock.
                        case None => {
                            order + ((quote.stock.symbol, (quote, quantity)))
                        }
                    }
                    currentQuote = None
                }

                // This shouldn't be visible if the user didn't enter a value.
                case None =>
                    throw BadInput("An unknown error has occurred.")
             } 
        }
    }

    def renderQuote(in: NodeSeq): NodeSeq = {
        (currentQuote match {
            case Some(quote) => (
                  ".quote-company *"    #> quote.company
                & ".quote-ticker *"     #> quote.stock.symbol
                & ".quote-price *"      #> quote.price.toString
                & ".quote-change *"     #> tryGetNumber(quote.info.percentChange)
                & ".quote-open *"       #> tryGetNumber(quote.info.openPrice)
                & ".quote-low *"        #> tryGetNumber(quote.info.lowPrice)
                & ".quote-high *"       #> tryGetNumber(quote.info.highPrice)
                & ".quote-dividend *"   #> tryGetNumber(quote.info.dividendShare)
                & ".quote-graph [src]"  #> "http://ichart.finance.yahoo.com/instrument/1.0/%s/chart;range=1d/image;size=239x110"
                                             .format(quote.stock.symbol toLowerCase)
            )

            case None =>
                ( "#search-quote" #> Nil
                & "#search-buy"   #> Nil)
                //same
        })(in)
    }

    def renderList(in: NodeSeq): NodeSeq = {
        (if (order isEmpty) {
            "#search-list" #> Nil
            //same
        } else {
            ( "#search-list-row" #> (order map (_ match {
                case (_, (quote, quantity)) => {
                    // TODO: What if the user doesn't have enough money?
                    val shares = BigDecimal((quantity / quote.price).toInt)
                    ( ".search-list-ticker *"   #> quote.stock.symbol
                    & ".search-list-company *"  #> quote.company
                    & ".search-list-price *"    #> (quote.price.$)
                    & ".search-list-shares *"   #> shares.toString
                    & ".search-list-subtotal *" #> ((shares * quote.price).$))
                }
              }))
            & ".search-list-total *" #> ("$" + getTotalPrice(order.values).toString))
        })(in)
    }

    private def tryGetNumber(a: Option[BigDecimal]): String = {
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

