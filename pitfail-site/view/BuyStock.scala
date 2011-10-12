
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
import comet._

class BuyStock extends Refreshable with Loggable
{
    object hub extends RefreshHub
    def registerWith = hub
    
    override def render = form.render  _
    
    case class Order(
        ticker: String,
        volume: BigDecimal
        )
    object Order {
        def fromHList(hl: String :+: BigDecimal :+: HNil) = hl match {
            case t :+: v :+: HNil => Order(t, v)
        }
    }
    
    object form extends Form[Order](hub,
        AggregateField(Order.fromHList _,
                StringField("ticker", "")
            :^: NumberField("volume", "10.00")
            :^: KNil
        )
    )
    {
        def act(order: Order) {
            userBuyStock(order.ticker, order.volume)
            Portfolio ! Refresh
        }
    }
    
    def userBuyStock(ticker: String, volume: BigDecimal) {
        import control.{BuyStock => Buyer}
        import Buyer._
        import control.LoginManager.NotLoggedIn
        import comet.NewsHub
        
        if (volume < 0)
            throw throw BadInput("You need to by more than $0.00 of the stock")
        
        try {
            Buyer.userBuyStock(ticker, volume)
            NewsHub()
        }
        catch {
            case NotEnoughCash(have, need) => throw BadInput(
                "You need at least %s you only have %s" format (
                    need toDollarString,
                    have toDollarString
                )
            )
            case NotLoggedIn() =>
                throw BadInput("You must be logged in to buy stock")
                
            case e => throw e
        }
    }
}

