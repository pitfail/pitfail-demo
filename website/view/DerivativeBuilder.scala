
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http.{StringField => _,BooleanField => _, _}
import js._
import JsCmds._
import JE._
import Helpers._

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.math._
import intform._

import stockdata._
import scalaz.Scalaz._
import formats._

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}

import model.schema._
import model._

abstract class Recipient
case class SpecificUser(portfolio: Portfolio) extends Recipient
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
    condition:   Condition,
    early:       Boolean
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
            cond: Condition,
            early: Boolean
        ) =>
            DerivativeOrder(
                recipient  = rec,
                price      = price,
                stocks     = stocks,
                execDate   = exp,
                cash       = cashDir.sign(strike),
                condition  = cond,
                early      = early
            )
        ,
        (
            toField,
            priceField: Field[Dollars],
            expirationField: Field[DateTime],
            strikePriceField: Field[Dollars],
            cashDirField,
            stocksField,
            conditionField,
            earlyField: Field[Boolean]
        ),
        <div id="search-derivative" class="block">
            <h2>Offer Derivative</h2>
            <p>Begin by selecting whether you want to sell this derivative to a
            specific user or to a public auction. If you choose to sell the
            derivative in a public auction you will have the opportunity to
            close the auction at any time. If you sell the derivative to a
            specific user they will have one day to accept or decline the
            offer.</p>

            <div id="derivative-recipient" class="builder-block">
                <h3>Buyer</h3>
                {toField.main}
                <dl>
                    <dt><label for="derivative-money-now">Price</label></dt>
                    <dd>${priceField.main & <input id="derivative-money-now" class="price blank"/>}</dd>
                </dl>
            </div>
            <div id="derivative-money" class="builder-block">
                <h3>Cash Transfers</h3>
                <dl>
                    <dt><label for="derivative-money-then">Strike Price</label></dt>
                    <dd>
                        ${strikePriceField.main & <input id="derivative-money-then" class="price blank"/>}
                        {cashDirField.main}
                    </dd>
                </dl>
            </div>
            <div style="clear:both;"/>
            <div id="derivative-expiration" class="builder-block">
                <h3>Exercise Date</h3>
                <dl>
                    <dt><label for="derivative-expiration">Date:</label></dt>
                    <dd>{expirationField.main & <input id="derivative-expiraiton" class="date blank"/>}</dd>

                    {earlyField.main}
                </dl>
            </div>
            <div id="derivative-conditions" class="builder-block">
                <h3>Conditions</h3>
                {conditionField.main}
            </div>
            <div style="clear:both;"/>

            <h3>Stocks</h3>
            <table class="derivative-stock-list boxy">
                <thead>
                    <tr>
                        <th class="search-list-ticker">Ticker</th>
                        <th class="search-list-company">Company</th>
                        <th class="search-list-shares">Shares</th>
                        <th class="search-list-dir"/>
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

    lazy val toField = CaseField[Recipient](
        Seq(
            toUserField,
            ConstField(OpenAuction)
        ),
        choices =>
            <ul>
                <li>
                    {choices._1 & <input id="to-user" checked="checked"/>}
                    <label for="to-user">User:</label>
                    {toUserField.main & <input id="to-user-name"/>}
                </li>
                <li>
                    {choices._2 & <input id="to-auction"/>}
                    <label for="to-auction">Public Auction</label>
                </li>
            </ul>
    )

    lazy val recipientField = new PortfolioField("") with FieldErrorRender
    lazy val toUserField = AggregateField(
        SpecificUser,
        recipientField: PortfolioField,
        <span>{recipientField.main & <input class="blank"/>}</span>
    )

    // Default to a week in the future.
    val tomorrow = DateTime.now().plusDays(7)
    lazy val expirationField = new DateTimeField(tomorrow, formatter) with FieldErrorRender

    // We can't really pick a good default
    lazy val priceField = new DollarsField("") with FieldErrorRender
    lazy val strikePriceField = new DollarsField("") with FieldErrorRender
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
                <td class="search-list-shares">{sharesField.main & <input class="blank shares"/>}</td>
                <td class="search-list-dir">{dirField.main}</td>
                <td class="search-list-price">{quote.price.$}/sh</td>
                <td class="search-list-buttons">
                    {remove.main & <input class="search-list-remove"/>}
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
            <p>
                {useField.main & <input id="condition-en"/>}
                <div class="chain">
                    <label for="condition-en" class="description">Provided that</label>
                </div>
                <div class="chain">
                    <label for="condition1" class="field-annotation">ticker or $</label>
                    {aField.main & <input id="condition1" class="blank condition"/>}
                </div>
                <div class="chain">&lt;</div>
                <div class="chain">
                    <label for="condition2" class="field-annotation">ticker or $</label>
                    {bField.main & <input id="condition2" class="blank condition"/>}
                </div>
            </p>
    }
    
    lazy val earlyField = new BooleanField(true) {
        override def main =
            <dt><label for="derivative-early">Exercise Early?</label></dt> ++
            <dd>{super.main & <input id="derivative-early"/>}</dd>
    }
    
    lazy val offerSubmit = Submit(form, "Offer")  { order =>
        import control.LoginManager._
        import control.PortfolioSwitcher._
        
        try {
            val expires = (new DateTime).plusDays(3)
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
                early      = order.early
            )
            
            val myPort = currentPortfolio
            order.recipient match {
                case SpecificUser(recip) =>
                    myPort.userOfferDerivativeTo(recip, deriv, order.price)
                    
                case OpenAuction =>
                    myPort.userOfferDerivativeAtAuction(deriv,
                        order.price, expires
                    )
            }
            
            comet.Portfolio        ! comet.Refresh
            comet.News             ! comet.Refresh
            comet.Offers           ! comet.Refresh
            comet.OutgoingOffers   ! comet.Refresh
            comet.AuctionThumbnail ! comet.Refresh
            
            clearAll()
        }
        catch {
            case NotLoggedIn =>
                throw BadFieldInput(recipientField, "You're not logged in")
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

