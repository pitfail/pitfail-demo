
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
import intform._

import model.derivatives._
import model.Schema.User

class SellDerivative extends Page with Loggable
{
    def render =
        <div class="sellDerivative">
            {form.render}
        </div>
    
    case class Order(
        to:         To,
        securities: Seq[Security],
        on:         String,
        condition:  Condition
    )
    
    abstract class To
    case class ToUser(user: User) extends To
    case object ToAuction extends To
    
    lazy val form: Form[Order] = Form(Order, (
            -toField,
            -secsField,
            -execField,
            -condField,
            -sellSubmit
        ))(F( (to,sec,on,cond,sub) =>
            <table class="sellDerivative">
                <tr><td>To:</td>
                    <td>{to.main}</td>
                    <td>{to.errors}</td>
                </tr>
                <tr><td>Securities:</td>
                    <td>{sec.main}
                        {sec.errors}
                    </td>
                </tr>
                <tr><td>On:</td>
                    <td>{on.main}</td>
                    <td>{on.errors}</td>
                </tr>
                <tr><td>If:</td>
                    <td>{cond.main}</td>
                    <td>{cond.errors}</td>
                </tr>
                
                <tr><td>{sub.main}</td></tr>
                <tr><td>{sub.errors}</td></tr>
            </table>
        ))
    
    lazy val toField = CaseField[To]((
            recipientField,
            ConstField(ToAuction)
        ))(C( (user,auc) =>
            <ul>
                <li>{user.choice} User: {user.main} {user.errors}</li>
                <li>{auc.choice} Auction</li>
            </ul>
        ))
        
    lazy val recipientField = AggregateField(ToUser, (
            -UserField()
        ))(F( (user) =>
            <span>To user: {user.main} {user.errors}</span>
        ))
        
    lazy val secsField = ConstField(Seq[Security]())
        
    lazy val execField = StringField()

    lazy val condField = CaseField[Condition]((
            ConstField(CondAlways),
            ifField
        ))(C( (al,iff) =>
            <ul>
                <li>{al.choice} Always</li>
                <li>{iff.choice} If {iff.main} {iff.errors}</li>
            </ul>
        ))

    lazy val ifField = ConstField(CondAlways)
    
    lazy val sellSubmit = Submit(form, "Sell") { order =>
        import model.Schema._
        import control.LoginManager._
        import org.joda.time.DateTime
        
        try {
            val deriv = Derivative(
                order.securities,
                new DateTime, // TODO: Change this!
                order.condition,
                true
            )
        
            val user = currentUser
            order.to match {
                case ToUser(recip) =>
                    user.offerDerivativeTo(recip, deriv)
                    
                case ToAuction =>
                    user.offerDerivativeAtAuction(deriv)
            }
            
            comet.Portfolio ! comet.Refresh
            comet.News      ! comet.Refresh
            comet.Offers    ! comet.Refresh
            
            form.reset()
        }
        catch {
            case NotLoggedIn =>
                throw BadInput("You must be logged in to trade derivatives")
        }
    }
}

