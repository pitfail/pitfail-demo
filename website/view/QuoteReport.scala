
package code
package snippet

import java.math.{MathContext,RoundingMode}

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http.{StringField => _, _}
import js._
import JsCmds._
import JE._
import Helpers._

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.math.{BigDecimal}
import intform._

import stockdata._
import model.StockPriceSource
import scalaz.Scalaz._

import control.LoginManager.loggedIn_?

import org.joda.time.Duration

import formats._

object quoteReport extends Loggable {
//

def apply(quote: Quote): NodeSeq = {
    def quoteBlockPresent = 
        <div id="search-quote" class="quote block">
            <h2>{quote.stock.symbol} - {quote.company}</h2>
            <h3>
                <span class="quote-price">{quote.price.$}</span> -
                <span class="quote-change">{quote.info.percentChange.%()}</span>
            </h3>
            <dl> {
                import quote.info._
                (<dt>Open</dt>    <dd class="quote-open">{openPrice.$}</dd>
                <dt>Low</dt>      <dd class="quote-low">{lowPrice.$}</dd>
                <dt>High</dt>     <dd class="quote-high">{highPrice.$}</dd>
                <dt>Dividend</dt> <dd class="quote-dividend">{dividendShare.$}</dd>)
            } </dl>
            {quoteGraph}
        </div>

    def quoteGraph =
        <img
            class="quote-graph"
            alt="stock price over time"
            src={graphURL}
        />
        
    def graphURL =
        "http://ichart.finance.yahoo.com/instrument/1.0/%s/chart;range=1d/image;size=239x110"
         .format(quote.stock.symbol toLowerCase)
         
    quoteBlockPresent
}
//
}

