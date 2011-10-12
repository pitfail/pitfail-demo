
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

import model.derivatives._
import model.Schema

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
    object Order {
        def fromHList(
            hl: To:+:Seq[Security]:+:String:+:Condition:+:HNil
        ): Order = hl match {
            case t:+:s:+:o:+:c:+:HNil => Order(t, s, o, c)
        }
    }
    
    def makeSecStock(hl: String:+:BigDecimal:+:HNil) = hl match {
        case t:+:s:+:HNil => SecScale(SecStock(t), s)
    }
    
    abstract class To
    case class ToUser(user: String) extends To
    object ToUser {
        def fromHList(hl: String:+:HNil) = hl match {
            case u:+:HNil => ToUser(u)
        }
    }
    
    case object ToAuction extends To
    
    object form extends Form[Order](hub,
        AggregateField(Order.fromHList _,
                CaseField[To]("to",
                    "user" -> AggregateField(ToUser.fromHList _,
                                      StringField("recipient", "")
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
                    "always" -> ConstField(CondAlways),
                    "when"   -> ConstField(CondAlways)
                )
            :^: KNil
        )
    )
    {
        override def act(order: Order) {
            userSellDerivative(order)
        }
    }
    
    def userSellDerivative(order: Order) {
        val deriv = Derivative(
            SecSum(order.securities),
            Schema.now, // TODO: Change this!
            order.condition
        )
        
        // TODO: This
    }
}


