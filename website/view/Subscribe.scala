
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import Helpers._

import formats._
import intform._
import errors._

import model.schema._

class Subscribe extends Page with Loggable {
    def render = refreshable.render
    val refreshable = Refreshable(doRender)
    
    var state: State = Pending
    
    def doRender: NodeSeq =
        state match {
            case Pending    => renderPending
            case Subscribed => renderSubscribed
        }
    
    def renderPending = {
        lazy val form: Form[Order] = Form(
            Order,
            (
                emailField
            ),
            <div>
                <h2>Subscribed to our Newsletter!</h2>
                <p>Email: {emailField.main & <input class="blank"/>}
                    {submit.main} {submit.errors}</p>
            </div>
        )
        lazy val emailField = StringField("")
        lazy val submit = Submit(form, "Subscribe") { case Order(email) =>
            import control.LoginManager._
            
            try {
                currentUser.userSubscribeToNewsletter(email)
                state = Subscribed
                refreshable.refresh
            }
            catch {
                case NotLoggedIn => throw BadInput("You need to be logged in to subscribe")
            }
        }
    
        form.render
    }
    
    def renderSubscribed = {
        <p>Sweet!</p>
    }
    
    case class Order(email: String)
    
    sealed trait State
    case object Pending extends State
    case object Subscribed extends State
}



