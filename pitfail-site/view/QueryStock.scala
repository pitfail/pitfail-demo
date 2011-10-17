
package code
package snippet

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
            currentQuote = Some(stockDatabase.getQuotes(Iterable(stock)).head)
        }
    }

    object quoteForm extends Form[BigDecimal](
        NumberField("quantity", "1"),
        formID = Some("search-quote")
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
                  "#search-company *"  #> quote.company
                & "#search-ticker *"   #> quote.stock.symbol
                & "#search-price *"    #> quote.price.toString
                & "#search-change *"   #> tryGetNumber(quote.info.percentChange)
                & "#search-open *"     #> tryGetNumber(quote.info.openPrice)
                & "#search-low *"      #> tryGetNumber(quote.info.lowPrice)
                & "#search-high *"     #> tryGetNumber(quote.info.highPrice)
                & "#search-dividend *" #> tryGetNumber(quote.info.dividendShare)
            )

            case None =>
                ("#search-quote" #> Nil)
        })(in)
    }

    def renderList(in: NodeSeq): NodeSeq = {
        (if (order isEmpty) {
            "#search-list" #> Nil
        } else {
            ( "#search-list-row" #> (order map (_ match {
                case (_, (quote, quantity)) => (
                      ".search-list-ticker *"   #> quote.stock.symbol
                    & ".search-list-company *"  #> quote.company
                    & ".search-list-price *"    #> ("$" + quote.price.toString)
                    & ".search-list-shares *"   #> quantity.toString
                    & ".search-list-subtotal *" #> ("$" + (quote.price * quantity).toString)
                )
              }))
            & ".search-list-total *" #> ("$" + getTotalPrice(order.values).toString))
        })(in)
    }

    private def tryGetNumber(a: Option[BigDecimal]): String = {
        a match {
            case Some(number) => number.toString
            case None         => "n/a"
        }
    }

    private def getTotalPrice(quotes: Iterable[(Quote, BigDecimal)]): BigDecimal = {
        (quotes map (_ match {
            case (quote, quantity) => quote.price * quantity
        })).reduceLeft[BigDecimal](_ + _)
    }
}

