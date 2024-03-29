
// Written by: Michael Koval

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
import scalaz.Scalaz._
import formats._

class SearchPipeline extends Page with Loggable
{
    val searchForm = new SearchBar()
    val actionForm = new StockOrderer()
    val derivativeForm = new DerivativeBuilder()

    searchForm.listen(_ match {
        case searchForm.HaveQuote(quote) =>
            actionForm.changeQuote(quote) 

        case _ =>
            actionForm.clearQuote & Focus("search-query-field")
    })

    actionForm.listen(_ match {
        case _: NoOrder =>
            searchForm.clear & Focus("search-query-field")

        case _: BuyShares =>
            searchForm.clear & Focus("search-query-field")

        case order: AddToDerivative => {
            searchForm.clear & derivativeForm.addOrder(order) & Focus("to-user-name")
        }
    })

    private val refreshable = Refreshable(
        <div id="search" class="container">
            {searchForm.render}
            {actionForm.render}
            {derivativeForm.render}
        </div>
    )

    def render = refreshable.render
}

