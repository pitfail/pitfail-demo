
package code
package comet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import control.LoginManager
import LoginManager.{currentLogin}

import model.Schema._
import formats._
import intform._

class Offers extends Refreshable with Loggable
{
    def registerWith = Offers
    
    def render = (in: NodeSeq) => trans {
        for {
            name <- currentLogin
            user <- byUsername(name)
            
            port = user.mainPortfolio
            myOffers = port.myOffers
        } yield {
            import snippet._
            
            def result =
                <div> {
                    if (!myOffers.isEmpty) offers
                    else Nil: NodeSeq
                } </div>
            
            def offers =
                <h4 id="ifHaveOffers">You have an offer:</h4> ++
                <ul class="offers">
                    {myOffers map offer _}
                </ul>
            
            def offer(o: DerivativeOffer) =
                <li>
                    {UserLink(o.from.owner.username)}
                    is offering
                    {o.derivative toHumanString}
                    {acceptOffer(o.handle)}
                    {declineOffer(o.handle)}
                </li>
            
            result
        }
    } getOrElse <span/>
    
    def acceptOffer(offerID: String) = FormSubmit.rendered("Accept") {
        import control.LoginManager._
        
        try {
            val user = currentUser
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
    
    def declineOffer(offerID: String) = FormSubmit.rendered("Decline") {
        import control.LoginManager._
        
        try {
            val user = currentUser
            user.declineOffer(offerID)
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

object Offers extends RefreshHub

