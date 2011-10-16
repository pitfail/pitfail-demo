package code
package comet

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

class QueryStock extends Refreshable with Loggable
{
    object hub extends RefreshHub
    def registerWith = hub

    val stockDatabase: StockDatabase = new YahooStockDatabase(new HttpQueryService("GET"))
    private object currentStock extends SessionVar[Option[Stock]](None)
    private object pendingStocks extends SessionVar[List[Stock]](List())
    
    object form extends Form[Stock](hub,
        AggregateField((ticker: String) => Stock("NasdaqNM", ticker),
                StringField("query", "")
            :^: KNil
        )
    )
    {
        override def act(stock: Stock) {
            currentStock(Some(stock))
            hub ! Refresh
        }
    }

    override def render = (doRender _) andThen (form.render _)
    
    def doRender(in: NodeSeq): NodeSeq = {
        (currentStock.is match {
            case Some(stock) => {
                val quote = stockDatabase.getQuotes(Iterable(stock)).head
                ("#search-quote" #> quote.price.toString & "#search-change" #> "(-1%)")
            }
            case None        => same
        })(in)
    }
}


