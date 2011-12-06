
package code
package snippet

import net.liftweb.common.Loggable
import scala.xml._

import intform._

import net.liftweb.http._
import js._
import JsCmds._
import JE._

import model._
import model.schema._

class StockSeller extends Loggable {
    import StockSeller._
    
    var state: State = Idle
    var defaultDollars: Dollars = Dollars(0)
    
    def doRender: NodeSeq = {
        state match {
            case Idle       => Nil
            case Selling(t) => renderSelling(t)
        }
    }
    
    def renderSelling(ticker: String) = {
        lazy val main =
            <div class="stock-seller">
                <p>Selling {ticker}</p>
                {sellForm.render}
                {availableBuyers(ticker)}
            </div>
            
        lazy val sellForm: Form[Order] = Form(
            Order,
            (
                dollarsField
            ),
            <p>Sell {dollarsField.main}$ {dollarsField.errors}
                {submit.main} {submit.errors}
            </p>
        )
        
        lazy val dollarsField = DollarsField(defaultDollars.no$)
        
        lazy val submit = Submit(sellForm, "Sell") { case Order(dollars) =>
            import control.LoginManager._
            import control.PortfolioSwitcher._
            
            try {
                currentPortfolio.userSellStock(ticker, dollars)
                state = Idle
                refreshable.refresh
            }
            catch {
                case NotLoggedIn => throw BadInput("You need to be logged in")
                case NoBidders   => throw BadInput("There are not enough bidders"
                    + " to accomodate your order")
                case NotEnoughShares(have, need) => throw BadInput("You only have %s"
                    + " shares of %s" format (have.###(), ticker))
            }
        }
        
        main
    }
    
    def activate(ticker: String, dollars: Dollars): JsCmd = {
        state = Selling(ticker)
        defaultDollars = dollars
        refreshable.refresh
    }

    def render = refreshable.render
    lazy val refreshable = Refreshable(doRender)
}

object StockSeller {
    def apply() = new StockSeller()
    
    sealed trait State
    case object Idle extends State
    case class Selling(ticker: String) extends State

    case class Order(dollars: Dollars)

}

