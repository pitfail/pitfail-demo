
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
import view.UserField

import model.derivatives._
import model.Schema.User

class SellDerivative extends Refreshable with Loggable
{
    object hub extends RefreshHub
    def registerWith = hub
    
    override def render = form.render  _
    
    case class Order(
        to:         To,
        securities: Seq[Security],
        on:         String,
        condition:  Condition
    )
    
    def makeSecStock(t: String, s: BigDecimal) =
        SecScale(SecStock(t), s)
    
    abstract class To
    case class ToUser(user: User) extends To
    case object ToAuction extends To
    
    object form extends Form[Order](hub,
        AggregateField(Order,
                CaseField[To]("to",
                    "user" -> AggregateField(ToUser,
                                      UserField("recipient", "")
                                  :^: KNil
                              ),
                    "auction" -> ConstField(ToAuction)
                )
            :^: ListField[Security]("securities", hub,
                    AggregateField(makeSecStock _,
                            StringField("ticker", "")
                        :^: NumberField("shares", "1")
                        :^: KNil
                    )
                )
            :^: StringField("on", "February 42th")
            :^: CaseField[Condition]("cond",
                    "always" -> ConstField(CondAlways()),
                    "when"   -> ConstField(CondAlways())
                )
            :^: KNil
        )
    )
    {
        override def act(order: Order) {
            userSellDerivative(order)
            Offers ! Refresh
        }
    }
    
    def userSellDerivative(order: Order) {
        import model.Schema._
        import control.LoginManager._
        import org.joda.time.DateTime
        
        try {
            val deriv = Derivative(
                SecSum(order.securities),
                new DateTime, // TODO: Change this!
                order.condition
            )
        
            val user = currentUser
            order.to match {
                case ToUser(recip) =>
                    user.offerDerivativeTo(recip, deriv)
                    
                case ToAuction =>
                    user.offerDerivativeAtAuction(deriv)
            }
        }
        catch {
            case NotLoggedIn =>
                throw BadInput("You must be logged in to trade derivatives")
        }
    }
}

