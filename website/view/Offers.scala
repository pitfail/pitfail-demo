
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
            
            def result: NodeSeq =
                if (myOffers isEmpty) 
                    Nil
                else
                    <div id="offers" class="container block">
                        <h2>Pending Offer</h2>
                        {
                        if (myOffers.length == 1)
                            <p>Another user has offered to sell you a derivative.
                            Look at the table below for more information and
                            choose whether to accept or decline this offer.</p>
                        else 
                            <p>Other users have offered to sell you
                            derivatives. Look at the table below for
                            information about the derivatives and choose
                            whether to accept or decline the offers.</p>
                        }
                        <table class="boxy">
                            <col class="from"/>
                            <col class="securities"/>
                            <col class="expiration"/>
                            <col class="condition"/>
                            <col class="strike-price"/>
                            <col class="buttons"/>
                            <thead>
                                <tr>
                                    <th>From</th>
                                    <th>Securities</th>
                                    <th>On</th>
                                    <th>If</th>
                                    <th>For</th>
                                    <th/>
                                </tr>
                            </thead>
                            <tbody>
                                {myOffers map offer _}
                            </tbody>
                        </table>
                    </div>
            
            def offer(o: DerivativeOffer) = {
                val deriv = o.derivative
                
                <tr>
                    <td>{UserLink(o.from.owner.username)}</td>
                    <td>{deriv.securities toHumanString}</td>
                    <td>{deriv.exec toNearbyString}</td>
                    <td>{deriv.condition toHumanString}</td>
                    <td>{o.price.$}</td>
                    <td>{acceptOffer(o.handle)} {declineOffer(o.handle)}</td>
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

