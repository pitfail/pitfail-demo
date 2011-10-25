
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
                <div id="offers"> {
                    if (!myOffers.isEmpty) offers
                    else Nil: NodeSeq
                } </div>
            
            def offers =
                <h3 id="ifHaveOffers">You have an offer:</h3> ++
                <table class="offers">
                    <tr>
                        <th>From</th>
                        <th>Securities</th>
                        <th>On</th>
                        <th>If</th>
                        <th>For</th>
                    </tr>
                    {myOffers map offer _}
                </table>
            
            def offer(o: DerivativeOffer) = {
                val deriv = o.derivative
                
                <tr>
                    <td>{UserLink(o.from.owner.username)}</td>
                    <td>{deriv.securities toHumanString}</td>
                    <td>{deriv.exec toNearbyString}</td>
                    <td>{deriv.condition toHumanString}</td>
                    <td>{"todo"}</td>
                    <td>{acceptOffer(o.handle)}</td>
                    <td>{declineOffer(o.handle)}</td>
                </tr>
            }
            
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

