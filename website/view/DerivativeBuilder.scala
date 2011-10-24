
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
import scala.math._
import intform._

import stockdata._
import model.derivatives._
import model.Schema.User
import scalaz.Scalaz._
import formats._

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}

abstract class Recipient
case class SpecificUser(user: User) extends Recipient
case object OpenAuction extends Recipient

sealed abstract class Direction {
    def sign(x: BigDecimal): BigDecimal
    def sign(x: Int): Int
}

case object ToBuyer  extends Direction {
    def sign(x: BigDecimal) = x
    def sign(x: Int) = x
}
case object ToSeller extends Direction {
    def sign(x: BigDecimal) = -x
    def sign(x: Int) = -x
}

case class StockInDerivative(
    quote:     Quote,
    shares:    Int,
    direction: Direction
)

case class DerivativeOrder(
    recipient:   Recipient,
    stocks:      Iterable[StockInDerivative],
    execDate:    DateTime,
    price:       BigDecimal,
    cash:        BigDecimal
)

class DerivativeBuilder extends Page with Loggable
{
    private val formatter = DateTimeFormat.forPattern("MM/dd/yyyy");
    private var listeners: List[Option[Quote] => JsCmd] = Nil;
    
    // Whether this is currently being used
    private var active: Boolean = false

    private val refreshable = Refreshable (
        if (active) form.render
        else Nil
    )

    implicit def toDate(str: String) = new {
        def toDate: DateTime =
            formatter.parseDateTime(str)
    }

    implicit def toDollars(number: Option[BigDecimal]) = new {
        def $: String = {
            (number map (_.$)) getOrElse "n/a" 
        }

        def %(): String = {
            (number map (_.%())) getOrElse "n/a"
        }
    }

    def directionToLabel(d: Direction) = d match {
        case ToBuyer  => "To Buyer"
        case ToSeller => "To Seller"
    }

    lazy val form: Form[DerivativeOrder] = Form(
        (
            rec: User,
            price: BigDecimal,
            exp: DateTime,
            strike: BigDecimal,
            cashDir: Direction,
            stocks: Seq[StockInDerivative]
        ) =>
            DerivativeOrder(
                recipient  = SpecificUser(rec),
                price      = price,
                stocks     = stocks,
                execDate   = exp,
                cash       = cashDir.sign(strike)
            )
        ,
        (
            recipientField,
            priceField,
            expirationField,
            strikePriceField,
            cashDirField,
            stocksField
        ),
        <div id="search-derivative" class="block">
            <h2>Offer Derivative</h2>
            <p>Offer to enter a contract with {recipientField.main & <input/>}
            {recipientField.errors} for the price of ${priceField.main & <input
            class="price"/>}.</p>
            
            <p>On the date {expirationField.main & <input class="date"/>} the
            following will be traded:</p> 
            
            <h3>Cash</h3>
            <p>${strikePriceField.main & <input class="price"/>}) {cashDirField.main}</p>
    
            <h3>Stocks</h3>
            
            <table class="block" id="search-list">
                <thead>
                    <tr>
                        <th class="search-list-ticker">Ticker</th>
                        <th class="search-list-company">Company</th>
                        <th class="search-list-shares">Shares</th>
                        <th class="search-list-dir"> </th>
                        <th class="search-list-price">Current Price</th>
                        <th class="search-list-buttons"/>
                    </tr>
                </thead>
                <tbody>
                    {stocksField.main}
                </tbody>
            </table>

            <div class="buttons">
                {offerSubmit.main & <input/>}
                {cancelSubmit.main & <input/>}
            </div>
        </div>
    )

    // TODO: This should allow a public auction.
    lazy val recipientField = UserField("")

    // Default to a week in the future.
    val tomorrow = DateTime.now().plusDays(7)
    lazy val expirationField = DateTimeField(tomorrow, formatter)

    // We can't really pick a good default
    lazy val priceField = NumberField("0.00")
    lazy val strikePriceField = NumberField(getTotalVolume.toString)
    lazy val cashDirField = DirectionField(ToSeller)

    lazy val stocksField: Field[Seq[StockInDerivative]] with FieldRender =
        DependentListField(
            stockRowFields.values toList,
            (stockRowFields map (_._2.main)).foldLeft[NodeSeq](Nil)(_ ++ _)
        )
    
    type StockRowField = Field[StockInDerivative] with FieldRender
    var stockRowFields: SortedMap[String,StockRowField] = TreeMap()
    
    def makeStockRowField(init: AddToDerivative): StockRowField = {
        val AddToDerivative(quote, shares) = init
        
        val sharesField = IntField(shares toString)
        val dirField = DirectionField(ToBuyer)
        
        val remove = Submit.cancel(form, "Remove") {
            stockRowFields -= quote.stock.symbol
            refreshable.refresh()
        }
        
        AggregateField(
            (sh: Int, dir: Direction) =>
                StockInDerivative(
                    quote     = quote,
                    shares    = sh,
                    direction = dir
                ),
            (
                sharesField: Field[Int],
                dirField
            ),
            <tr>
                <td class="search-list-ticker">{quote.stock.symbol}</td>
                <td class="search-list-company">{quote.company}</td>
                <td class="search-list-shares">{sharesField.main} {sharesField.errors}</td>
                <td class="search-list-dir">{dirField.main}</td>
                <td class="search-list-price">{quote.price.$}/sh</td>
                <td class="search-list-buttons">
                    {remove.main & <input class="search-list-remove"/>}
                    {remove.errors}
                </td>
            </tr>
        )
    }
    
    lazy val offerSubmit = Submit(form, "Offer")  { order =>
        import control.LoginManager._
        
        try {
            val stocks = order.stocks map {
                case StockInDerivative(quote, shares, dir) =>
                    SecStock(quote.stock.symbol, dir.sign(shares))
            } toList
            // TODO: Direction
            val secs = SecDollar(order.cash) :: stocks
            
            val deriv = Derivative(
                securities = secs,
                exec       = order.execDate,
                condition  = CondAlways,
                early      = true
            )
            
            val user = currentUser
            order.recipient match {
                case SpecificUser(recip) => user.offerDerivativeTo(recip, deriv)
                case OpenAuction         => user.offerDerivativeAtAuction(deriv)
            }
            
            comet.Portfolio ! comet.Refresh
            comet.News      ! comet.Refresh
            comet.Offers    ! comet.Refresh
            
            clearAll()
        }
        catch {
            case NotLoggedIn => throw BadInput("You're not logged in")
        }
    }
    
    lazy val cancelSubmit = Submit.cancel(form, "Cancel") {
        clearAll()
    }

    def DirectionField(init: Direction): SelectField[Direction] =
        SelectField(
            Seq(
                (ToBuyer, "To Buyer"),
                (ToSeller, "To Seller")
            ),
            init
        )
    
    private def clearAll() = {
        form.reset()
        stockRowFields = TreeMap()
        active = false
        refreshable.refresh()
    }
    
    private def notify(quote: Option[Quote]): JsCmd =
        (listeners map { (callback) => callback(quote) }).foldLeft(Noop)(_ & _)

    private def notifyAndRefresh(quote: Option[Quote]): JsCmd = {
        notify(quote) & refreshable.refresh
    }

    /*
     * Public API
     */
    def listen(callback: Option[Quote] => JsCmd) {
        listeners ::= callback
    }

    def addOrder(order: AddToDerivative): JsCmd = {
        stockRowFields = stockRowFields + ((
            order.quote.stock.symbol,
            makeStockRowField(order)
        ))
    
        active = true
        refreshable.refresh
    }

    // TODO: This is wrong
    def getTotalVolume: BigDecimal = BigDecimal("3.14")

    override def render = refreshable.render
}

