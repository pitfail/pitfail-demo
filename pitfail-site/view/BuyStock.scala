
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

class BuyStock extends RenderableSnippet with Loggable
{
    def dispatch = {
        case "render" => form.render _
    }
    
    case class Order(
            ticker: String,
            volume: BigDecimal
        )
    
    object form extends Form[Order](this,
        AggregateField(Order,
                StringField("ticker", "")
            :^: NumberField("volume", "10.00")
            :^: KNil
        )
    )
    {
        def act(order: Order) {
            userBuyStock(order.ticker, order.volume)
            comet.Portfolio ! comet.Refresh
            comet.News ! comet.Refresh
        }
    }
    
    def userBuyStock(ticker: String, volume: BigDecimal) {
        import control.LoginManager._
        import model.Schema._
        
        try {
            val user = currentUser
            user.mainPortfolio.buyStock(ticker, volume)
        }
        catch {
            case NegativeVolume => throw BadInput(
                "You must buy more than $0.00 of a stock"
            )
            case NotEnoughCash(have, need) => throw BadInput(
                "You need at least %s you only have %s" format (
                    need toDollarString,
                    have toDollarString
                )
            )
            case NotLoggedIn =>
                throw BadInput("You must be logged in to buy stock")
        }
    }
}

