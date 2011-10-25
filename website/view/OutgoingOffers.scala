
package code
package comet

import net.liftweb.{common, http, util}
import common._
import util._
import http._
import scala.xml._

import intform._
import model.Schema._
import formats._
import snippet.{UserLink}

class OutgoingOffers extends Refreshable with Loggable {
    
    def registerWith = OutgoingOffers
    
    def render = trans {
        import control.LoginManager._
        
        try {
            val user   = currentUser
            val port   = user.mainPortfolio
            val offers = port.myAuctionOffers
            
            lazy val all =
                <div id="outgoing-offers">
                    <h2>You have offers at auction:</h2>
                    
                    <table>
                        <tr>
                            <th></th>
                            <th>Going Price</th>
                            <th>Bid by</th>
                            <th>Expires on</th>
                            <th>Offering</th>
                        </tr>
                        {offerRows}
                    </table>
                </div>
            
            lazy val offerRows = offers map { offer =>
                val high = offer.highBid
                val highBidder = high map (_.by.owner.username)
                
                lazy val all =
                    <tr>
                        <td>{closeButton.render}</td>
                        <td>{offer.goingPrice.$}</td>
                        <td>{highBidder map (UserLink(_)) getOrElse <span>-</span>}</td>
                        <td>{offer.expires toNearbyString}</td>
                        <td>{offer.derivative toHumanString}</td>
                    </tr>
                
                lazy val closeButton = FormSubmit("Close") {
                    offer.close()
                    
                    OutgoingOffers   ! Refresh
                    Portfolio        ! Refresh
                    AuctionThumbnail ! Refresh
                }
                
                all
            }
            
            if (offers.isEmpty) Nil
            else all
        }
        catch {
            case NotLoggedIn => Nil
        }
    }
}

object OutgoingOffers extends RefreshHub

