
package code
package snippet

import net.liftweb.{common, http}
import common._
import http._

import scala.xml._

import intform._
import errors._
import Box._

import formats._
import model.schema._

class PortfolioInvites extends Page with Loggable {
    def render = refreshable.render
    val refreshable = Refreshable(doRender)
    
    def doRender: NodeSeq = {
        import control.LoginManager._
        
        try {
            val user = currentUser
            val invites = user.myPortfolioInvites
            
            invites match {
                case Nil     => (Nil: NodeSeq)
                case invites => formatInvites(user, invites)
            }
        }
        catch {
            case NotLoggedIn => (Nil: NodeSeq)
        }
    }
    
    def formatInvites(user: User, invites: Seq[PortfolioInvite]) = {
        val list = invites map { invite =>
            val accept = FormSubmit.rendered("Accept") {
                user.userAcceptInvite(invite)
                refreshable.refresh
            }
            
            val decline = FormSubmit.rendered("Decline") {
                user.userDeclineInvite(invite)
                refreshable.refresh
            }
        
            <li>{PortfolioLink(invite.from)} is asking you to join their team!
                <span class="button">{accept}</span> <span class="button">{decline}</span>
            </li>
        }
        
        <div class="block">
            <ul>{list}</ul>
        </div>
    }
}
object PortfolioInvites {
    def apply() = new PortfolioInvites
}

