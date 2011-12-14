
// Written by: Owen Healy

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

class Invites extends Page with Loggable {
    def render = refreshable.render
    val refreshable = Refreshable(doRender)
    
    def doRender: NodeSeq = {
        import control.LoginManager._
        
        try {
            val user = currentUser
            user.myPortfolioInvites map (formatPortfolioInvite(user, _))
            user.myReceivedInvites map (formatLeagueInvite(user, _))
        }
        catch {
            case NotLoggedIn => (Nil: NodeSeq)
        }
    }
    
    def formatPortfolioInvite(user: User, invite: PortfolioInvite) = {
        val accept = FormSubmit.rendered("Accept") {
            user.userAcceptInvite(invite)
            refreshable.refresh
        }
        
        val decline = FormSubmit.rendered("Decline") {
            user.userDeclineInvite(invite)
            refreshable.refresh
        }
    
        val message = {
            <p>{PortfolioLink(invite.from)} is asking you to join their team!
                <span class="button">{accept}</span> <span class="button">{decline}</span>
            </p>
        }
        
        <div class="block invite">
            <ul>{message}</ul>
        </div>
    }
    
    def formatLeagueInvite(user: User, invite: LeagueInvite) = {
        val accept = FormSubmit.rendered("Accept") {
            invite.accept()
            refreshable.refresh
        }
        
        val decline = FormSubmit.rendered("Decline") {
            invite.decline()
            refreshable.refresh
        }
    
        val message = {
            <p>{UserLink(invite.sender)} is asking you to join their team!
                <span class="button">{accept}</span> <span class="button">{decline}</span>
            </p>
        }
        
        <div class="block invite">
            <ul>{message}</ul>
        </div>
    }
}
object Invites {
    def apply() = new Invites
}

