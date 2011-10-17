
package code
package snippet

import code.comet._

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import matteform._

object AcceptOffer extends RenderableSnippet with Loggable
{
    def dispatch = {
        case "render" => form.render _
    }
    
    object form extends Form[String](this,
        AttrField("offerID")
    )
    {
        def act(offerID: String) {
            import control.LoginManager._
            import model.Schema._
            
            try {
                val user = control.LoginManager.currentUser
                user.acceptOffer(offerID)
            }
            catch {
                case NotLoggedIn =>
                    throw new BadInput("You're not logged in")
                    
                case OfferExpired =>
                    throw new BadInput("This offer has expired")
            }
        }
    }
}

object DeclineOffer extends RenderableSnippet with Loggable
{
    def dispatch = {
        case "render" => form.render _
    }
    
    object form extends Form[String](this,
        AttrField("offerID")
    )
    {
        def act(offerID: String) {
            import control.LoginManager._
            import model.Schema._
            
            try {
                val user = control.LoginManager.currentUser
                user.declineOffer(offerID)
            }
            catch {
                case NotLoggedIn =>
                    throw new BadInput("You're not logged in")
                    
                case OfferExpired =>
                    throw new BadInput("This offer has expired")
            }
        }
    }
}

