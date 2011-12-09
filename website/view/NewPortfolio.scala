
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

        /* Type works around scalac bug:
        [error] /home/nyx/pf/website/view/NewPortfolio.scala:38: type mismatch;
        [error]  found   : (intform.StringField, intform.Field[Seq[model.schema.User]] with intform.FieldRender, code.snippet.LeagueField with intform.FieldErrorRender)
        [error]  required: up.KList[intform.Field,?]
        [error]             (
        [error]             ^
        [error] one error found
        Compilation failed
        */
        val league_f : intform.TextField[League] = new LeagueField("") with FieldErrorRender

        lazy val form: Form[Stuff] = Form(Stuff,
            (
                nameField,
                inviteesField,
                league_f
            ),
            <div>
                <p>Name: {nameField.main} {nameField.errors}</p>
                <p>League: {league_f.main}</p>
                <p>Invite users:</p>
                {inviteesField.main}
                <p>{createButton.main}</p>
                <p>{createButton.errors}</p>
            </div>
        )
    
        lazy val createButton = Submit(form, "Create Portfolio") { stuff =>
            import control.LoginManager._
            
            val Stuff(name, invitees, league) = stuff
            
            try {
                val port = currentUser.userCreatePortfolio(name, league)
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
    
    case class Stuff(name: String, invitees: Seq[User], league: League)
}


