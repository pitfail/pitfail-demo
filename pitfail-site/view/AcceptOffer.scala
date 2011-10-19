
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

object AcceptOffer extends RefreshableSnippet with Loggable
{
    def render(p: RefreshPoint)(in: NodeSeq) = form.render(p)(in)
    
    object form extends Form[String](
        AttrField("offerID")
    )
    {
        def act(offerID: String) {
            import control.LoginManager._
            import model.Schema._
            import comet.Refresh
            
            logger.info("ACCEPTING OFFER " + offerID)
            
            try {
                val user = control.LoginManager.currentUser
                user.acceptOffer(offerID)
                comet.Offers ! Refresh
                comet.Portfolio ! Refresh
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

object DeclineOffer extends RefreshableSnippet with Loggable
{
    def render(p: RefreshPoint)(in: NodeSeq) = form.render(p)(in)
    
    object form extends Form[String](
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

