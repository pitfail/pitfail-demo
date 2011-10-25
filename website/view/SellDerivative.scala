
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http.{js}
import js._
import JsCmds._
import JE._
import Helpers._

import scala.math._
import org.joda.time._
import formats._
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
        on:         DateTime,
        early:      Boolean,
        condition:  Condition
    )
    
    abstract class To
    case class ToUser(user: User) extends To
    case object ToAuction extends To
    
    lazy val form: Form[Order] = Form(Order,
        (
            toField,
            secsField,
            execField,
            earlyField,
            condField
        ),
        <table class="sellDerivative">
            <tr><td>To:</td>
                <td>{toField.main}</td>
                <td>{toField.errors}</td>
            </tr>
            <tr><td>Securities:</td>
                <td>{secsField.main}</td>
            </tr>
            <tr><td>On:</td>
                <td>{execField.main} ({execField.format})</td>
                <td>{execField.errors}</td>
            </tr>
            <tr>
                <td/>
                <td>{earlyField.main} can be exercised early</td>
            </tr>
            <tr/>
            <tr><td>If:</td>
                <td>{condField.main}</td>
                <td>{condField.errors}</td>
            </tr>
            
            <tr><td>{sellSubmit.main}</td></tr>
            <tr><td>{sellSubmit.errors}</td></tr>
        </table>
    )
    
    lazy val toField = CaseField[To](
        (
            recipientField,
            ConstField(ToAuction)
        ),
        choices =>
            <ul>
                <li>{choices._1} User: {userField.main} {userField.errors}</li>
                <li>{choices._2} Auction</li>
            </ul>
    )
        
    lazy val recipientField = AggregateField(
        ToUser,
        userField,
        <span>{userField.main} {userField.errors}</span>
    )
    lazy val userField = UserField("")
        
    // This funny type is working around a Scala compiler bug.
    // No I don't understand it either.
    lazy val secsField: Field[Seq[SecStock]] with FieldRender =
        ListField(
            stockField,
            (items, add) => {
                <table>{items map (i =>
                    <tr>{i.field ++ i.delete}</tr>
                )}
                </table> ++
                <p>{add}</p>
            }
        )
    
    def stockField: Field[SecStock] with Renderable = {
        val sharesField = NumberField("1.00")
        val tickerField = StringField("")
        
        AggregateField(SecStock,
            (
                tickerField,
                sharesField
            ),
            <td>{sharesField.main} {sharesField.errors}</td> ++
            <td>{tickerField.main} {tickerField.errors}</td>
        )
    }
        
    lazy val execField = DateField()
    lazy val earlyField = BooleanField()

    lazy val condField = CaseField[Condition](
        (
            ConstField(CondAlways),
            ifField
        ),
        choices =>
            <ul>
                <li>{choices._1} Always</li>
                <li>{choices._2} If {ifField.main}</li>
            </ul>
    )

    lazy val ifField = ConditionField()
    
    lazy val sellSubmit = Submit(form, "Sell") { order =>
        import model.Schema._
        import control.LoginManager._
        import org.joda.time.DateTime
        
        try {
            val deriv = Derivative(
                order.securities,
                order.on,
                order.condition,
                order.early
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

