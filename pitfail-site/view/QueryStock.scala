
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
    private var currentStock: Option[Stock] = None
    private var pendingStocks: List[Stock]  = List()

    def render(p: RefreshPoint)(in: NodeSeq) = (
           in
        |> form.render(p) _
        |> renderData _
    )
    
    object form extends Form[Stock](
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

    def renderData(in: NodeSeq): NodeSeq = {
        (currentStock match {
            case Some(stock) => {
                val quote = stockDatabase.getQuotes(Iterable(stock)).head
                ( "#search-company *"  #> quote.company
                & "#search-ticker *"   #> quote.stock.symbol
                & "#search-quote *"    #> quote.price.toString
                & "#search-change *"   #> quote.info.percentChange.toString
                & "#search-open *"     #> quote.info.openPrice.toString
                & "#search-low *"      #> quote.info.lowPrice.toString
                & "#search-high *"     #> quote.info.highPrice.toString
                & "#search-dividend *" #> quote.info.dividendShare.toString)
            }
            case None => same
        })(in)
    }
}

