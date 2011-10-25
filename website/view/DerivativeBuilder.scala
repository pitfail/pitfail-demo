
package code
package snippet

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
import scala.math._
import intform._

import stockdata._
import model.derivatives._
import model._
import model.Schema._
import scalaz.Scalaz._
import formats._

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}

abstract class Recipient
case class SpecificUser(user: User) extends Recipient
case object OpenAuction extends Recipient

sealed abstract class Direction {
    def sign(x: Dollars): Dollars
    def sign(x: Shares): Shares
    def sign(x: Int): Int
}

case object ToBuyer  extends Direction {
    def sign(x: Dollars) = x
    def sign(x: Shares) = x
    def sign(x: Int) = x
}
case object ToSeller extends Direction {
    def sign(x: Dollars) = -x
    def sign(x: Shares) = x * Scale("-1")
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
    price:       Dollars,
    cash:        Dollars,
    condition:   Condition
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

    implicit def toDollars(number: Option[Dollars]) = new {
        def $: String = {
            number map (_.$) getOrElse "n/a" 
        }
    }

    def directionToLabel(d: Direction) = d match {
        case ToBuyer  => "To Buyer"
        case ToSeller => "To Seller"
    }

    lazy val form: Form[DerivativeOrder] = Form(
        (
            rec: Recipient,
            price: Dollars,
            exp: DateTime,
            strike: Dollars,
            cashDir: Direction,
            stocks: Seq[StockInDerivative],
            cond: Condition
        ) =>
            DerivativeOrder(
                recipient  = rec,
                price      = price,
                stocks     = stocks,
                execDate   = exp,
                cash       = cashDir.sign(strike),
                condition  = cond
            )
        ,
        (
            toField,
            priceField,
            expirationField,
            strikePriceField,
            cashDirField,
            stocksField,
            conditionField
        ),
        <div id="search-derivative" class="block">
            <h2>Offer Derivative</h2>
            
            <p>Offer to enter a contract with {toField.main}</p>
            
            <p>for the price of ${priceField.main & <input
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
            
            {conditionField.main}

            <div class="buttons">
                {offerSubmit.main & <input/>}
                {cancelSubmit.main & <input/>}
            </div>
        </div>
    )

    lazy val toField = CaseField[Recipient](
        Seq(
            toUserField,
            ConstField(OpenAuction)
        ),
        choices =>
            <ul>
                <li>{choices._1} User: {toUserField.main}</li>
                <li>{choices._2} Public Auction</li>
            </ul>
    )

    // TODO: This should allow a public auction.
    lazy val recipientField = UserField("")
    lazy val toUserField = AggregateField(
        SpecificUser,
        recipientField,
        recipientField.main ++ recipientField.errors
    )

    // Default to a week in the future.
    val tomorrow = DateTime.now().plusDays(7)
    lazy val expirationField = DateTimeField(tomorrow, formatter)

    // We can't really pick a good default
    lazy val priceField = DollarsField("0.00")
    // TODO: Default to current total volume
    lazy val strikePriceField = DollarsField("")
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
        
        val sharesField = IntField(shares.###())
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
    
    lazy val conditionField = new Field[Condition] with FieldRender {
        lazy val useField = BooleanField()
        
        lazy val aField = compSecField
        lazy val bField = compSecField
        
        def compSecField = new TextField[ComparableSecurity]("") {
            def produce() = OK {
                try {
                    CompSecDollar(Price(text))
                }
                catch { case _: NumberFormatException =>
                    CompSecStock(text)
                }
            }
        }
        
        def produce() = useField.process flatMap { use =>
            if (use)
                for {
                    a <- aField.process
                    b <- bField.process
                } yield OK(CondGreater(b, a))
            else
                Some(OK(CondAlways))
        } getOrElse ChildError
        
        def reset() {
            useField.reset
            aField.reset
            bField.reset
        }
        
        def main =
            <p>{useField.main} Provided that
                {aField.main} &lt; {bField.main}
            </p>
    }
    
    lazy val offerSubmit = Submit(form, "Offer")  { order =>
        import control.LoginManager._
        
        try {
            val stocks = order.stocks map {
                case StockInDerivative(quote, shares, dir) =>
                    SecStock(quote.stock.symbol, dir.sign(Shares(shares)))
            } toList
            // TODO: Direction
            val secs = SecDollar(order.cash) :: stocks
            
            val deriv = Derivative(
                securities = secs,
                exec       = order.execDate,
                condition  = order.condition,
                early      = true
            )
            
            val user = currentUser
            order.recipient match {
                case SpecificUser(recip) => user.offerDerivativeTo(recip, deriv, order.price)
                case OpenAuction         => user.offerDerivativeAtAuction(deriv, order.price)
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

    override def render = refreshable.render
}

