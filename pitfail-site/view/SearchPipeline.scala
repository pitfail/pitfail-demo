
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

class SearchPipeline extends Page with Loggable
{
    val searchForm = new SearchQuote()
    val actionForm = new SearchAction()

    searchForm.listen(_ match {
        case Some(quote) => actionForm.changeQuote(quote) 
        case None        => actionForm.clearQuote
    })

    private val refreshable = Refreshable(
        <div id="search" class="container">
            {searchForm.render}
            {actionForm.render}
        </div>
    )

    def render = refreshable.render
}
