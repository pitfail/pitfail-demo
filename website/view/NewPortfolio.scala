
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

class NewPortfolio extends Page with Loggable {
    def render = refreshable.render
    val refreshable = Refreshable(doRender)
    
    def doRender: NodeSeq = {
        val nameField = StringField("")
        // This type is to get around a Scala compiler bug....
        val inviteesField: Field[Seq[User]] with FieldRender = ListField[User](
            new UserField("") with FieldErrorRender,
            (items, add) => {
                val list = items map { item =>
                    <li>{item.field} {item.delete}</li>
                }
                <ul>{list}</ul>
                <p>{add}</p>
            }
        )
        
        lazy val form: Form[Stuff] = Form(Stuff,
            (
                nameField,
                inviteesField
            ),
            <div>
                <p>Name: {nameField.main} {nameField.errors}</p>
                <p>Invite users:</p>
                {inviteesField.main}
                <p>{createButton.main}</p>
                <p>{createButton.errors}</p>
            </div>
        )
    
        lazy val createButton = Submit(form, "Create Portfolio") { stuff =>
            import control.LoginManager._
            
            val Stuff(name, invitees) = stuff
            
            try {
                val port = currentUser.userCreatePortfolio(name)
                invitees map (port.userInviteUser(_))
                redirectTo("/")
            }
            catch {
                case NotLoggedIn =>
                    throw BadInput("You need to be logged in to create a portfolio")
                    
                case NameInUse =>
                    throw BadInput("Apparently someone took that name before you "
                        + "youtube.com/watch?v=o605DTjj7HU")
            }
        }
        
        <div class="block">
            {form.render}
        </div>
    }
    
    case class Stuff(name: String, invitees: Seq[User])
}


