
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
    private var order: List[(Quote, BigDecimal)]  = List()

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
        name = "submit-query"
    )
    {
        override def act(stock: Stock) {
            logger.info("QUERY: " + stock)
            currentQuote = Some(stockDatabase.getQuotes(Iterable(stock)).head)
        }
    }

    object quoteForm extends Form[BigDecimal](
        NumberField("quantity", "1"),
        name = "submit-quote"
    )
    {
        override def act(quantity: BigDecimal) {
            currentQuote match {
                case Some(quote) => {
                    order = order :+ (quote, quantity)
                    currentQuote = None
                }

                case None =>
                    throw BadInput("An unknown error has occurred.")
             } 
        }
    }

    def renderQuote(in: NodeSeq): NodeSeq = {
        (currentQuote match {
            case Some(quote) => {
                ( "#search-company *"  #> quote.company
                & "#search-ticker *"   #> quote.stock.symbol
                & "#search-price *"    #> quote.price.toString
                & "#search-change *"   #> quote.info.percentChange.toString
                & "#search-open *"     #> quote.info.openPrice.toString
                & "#search-low *"      #> quote.info.lowPrice.toString
                & "#search-high *"     #> quote.info.highPrice.toString
                & "#search-dividend *" #> quote.info.dividendShare.toString)
            }

            case None =>
                ("#search-quote" #> Nil)
        })(in)
    }

    def renderList(in: NodeSeq): NodeSeq = {
        (if (order isEmpty) {
            "#search-list" #> Nil
        } else {
            "#search-list-row" #> (order map (_ match {
                case (quote, quantity) => 
                    ( ".search-list-ticker *"   #> quote.symbol
                    & ".search-list-company *"  #> quote.company
                    & ".search-list-price *"    #> ("$" + quote.price.toString)
                    & ".search-list-shares *"   #> quantity.toString
                    & ".search-list-subtotal *" #> ("$" + (quote.price * quantity).toString))
            }))
        })(in)
    }
}

