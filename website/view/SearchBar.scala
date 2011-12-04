
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
import texttrading._

class SearchBar extends Page with Loggable
{
    sealed trait Status
    case object Idle extends Status
    case class HaveQuote(quote: Quote) extends Status
    case class HaveCommand(command: String, response: Seq[String]) extends Status
    
    private var status: Status = Idle
    
    private val refreshable = Refreshable (
       <div id="search" class="container">
            {queryForm.render}
            {feedback}
        </div>
    )
    
    /*
     * Listening
     */
    private var listeners: List[Status => JsCmd] = Nil;

    private def notify(status: Status, cmd: JsCmd = Noop): JsCmd =
        (listeners map { callback => callback(status) }).foldLeft(Noop)(_ & _)

    private def notifyAndRefresh(status: Status, cmd: JsCmd = Noop): JsCmd = {
        // This order is important because cmd may apply to the new contents.
        notify(status, Noop) & refreshable.refresh & cmd
    }

    /*
     * Public API
     */
    def listen(callback: Status => JsCmd) {
        listeners ::= callback
    }

    def changeQuote(stock: Stock): JsCmd = {
        status = HaveQuote(StockPriceSource.getQuotes(Iterable(stock)).head)
        notifyAndRefresh(status, Focus("search-quantity"))
    }
    
    def runCommand(command: String): JsCmd = {
        import control.LoginManager._
        
        val username =
            try
                currentUser.username
            catch {
                case NotLoggedIn => "Anonymous"
            }
            
        status = HaveCommand(command, TextTrader.runCommand(username, command, backend))
        notifyAndRefresh(status, Focus("search-query-field"))
    }

    def clear: JsCmd = {
        status = Idle
        notifyAndRefresh(status, Focus("search-query-field"))
    }

    override def render = refreshable.render
    
    /*
     * Search Form
     */
    lazy val queryForm: Form[String] = Form(
        identity[String],
        (
            tickerField: Field[String]
        ),
        <div id="search-query">
            <div id="search-query-field-hack">
                {tickerField.main & <input id="search-query-field"/>}
            </div>
            {submit.main & <input id="search-query-button"/>}
            {submit.errors}
        </div> ++
        <p>{tickerField.errors}</p>
    )
    
    lazy val tickerField = new StringField("")

    lazy val submit = Submit(queryForm, "Go") { fullText =>
        val text = fullText.trim
        if (text.indexOf(' ') != -1 || TextTrader.looksLikeCommand(text)) {
            queryForm.reset()
            runCommand(text)
        }
        else {
            try
                changeQuote(Stock(text))
            catch {
                case _: NoSuchStockException =>
                    logger.info("Failed to find " + text)
                    
                    throw BadFieldInput(
                        tickerField,
                        "There is no stock with symbol " + text + "."
                    )
            }
        }
    }
    
    /*
     * Commands
     */
    private val backend = new PitFailBackend
    
    def feedback =
        status match {
            case HaveQuote(quote) => quoteReport(quote)
            case HaveCommand(command, response) => commandReport(command, response)
            case Idle => instructions
        }
    
    /* TODO: if we are on a user page, put the username here. */
    def instructions =
        <div id="search-instructions" class="block">
            <ol>
                {
                    if (!loggedIn_?)
                        <li>Click "Login" on the top right to login via Twitter</li>
                }
                <li>Enter a ticker symbol into the search field above (Example: "MSFT")</li>
                <li>Choose an amount to buy (simple) or add to a derivative (more advanced).</li>
                <li>Manage your portfolio!</li>
            </ol>
        </div>
    
    def commandReport(command: String, response: Seq[String]) = {
        val lines = response map {l => <p>{l}</p>}
        <div class="block">
            <p>&gt; {command}</p>
            {lines}
        </div>
    }
}

