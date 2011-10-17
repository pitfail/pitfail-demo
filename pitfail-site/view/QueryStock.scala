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

class QueryStock extends RenderableSnippet with Loggable
{
    private val stockDatabase: StockDatabase = new YahooStockDatabase(new HttpQueryService("GET"))
    private var currentStock: Option[Stock] = None
    private var pendingStocks: List[Stock]  = List()

    def dispatch = {
        case "render" => form.render _
    }
    
    object form extends Form[Stock](this,
        AggregateField(Stock,
                StringField("query", "")
            :^: KNil
        )
    )
    {
        override def act(stock: Stock) {
            currentStock = Some(stock)
        }
    }

    override def render(in: NodeSeq): NodeSeq = {
        (currentStock match {
            case Some(stock) => {
                try {
                    val quote = stockDatabase.getQuotes(Iterable(stock)).head
                    ( "#search-company *"  #> quote.company
                    & "#search-ticker *"   #> quote.stock.symbol
                    & "#search-price *"    #> quote.price.toString
                    & "#search-change *"   #> quote.info.percentChange.toString
                    & "#search-open *"     #> quote.info.openPrice.toString
                    & "#search-low *"      #> quote.info.lowPrice.toString
                    & "#search-high *"     #> quote.info.highPrice.toString
                    & "#search-dividend *" #> quote.info.dividendShare.toString
                    & "#search-list"       #> Nil)
                } catch {
                    case e: DatabaseException =>
                        throw BadInput("Unable to get an up-to-date stock quote. Please try again later.")

                    case e: NoSuchStockException =>
                        throw BadInput(e.toString)

                    case _ =>
                        throw BadInput("An unknown error has occurred.")
                }
            }

            case None => {
                ( "#search-quote" #> Nil
                & "#search-list"  #> Nil)
            }
        })(in)
    }
}


