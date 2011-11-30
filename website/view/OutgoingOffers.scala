
package code
package comet

import net.liftweb.{common, http, util}
import common._
import util._
import http._
import scala.xml._

import intform._
import formats._
import snippet.{UserLink}
import model.schema._

class OutgoingOffers extends Refreshable with Loggable {
    
    def registerWith = OutgoingOffers
    
    def render = readDB {
        import control.LoginManager._
        
        try {
            val user   = currentUser
            val port   = user.mainPortfolio
            val offers = port.auctionOffers
            
            lazy val all =
                <div id="outgoing-offers" class="block">
                    <h2>Offers at Auction</h2>
                    <p>You have offered one or more derivatives to an open
                    auction. Monitor their current prices below and use the
                    "close" button to confirm the sale.</p>
                    
                    <table class="boxy">
                        <thead>
                            <tr>
                                <th/>
                                <th>Going Price</th>
                                <th>Bid by</th>
                                <th>Expires on</th>
                                <th>Offering</th>
                            </tr>
                        </thead>
                        <tbody>
                            {offerRows}
                        </tbody>
                    </table>
                </div>
            
            lazy val offerRows = offers map { offer =>
                val high = offer.highBid
                val highBidder = high map (_.by.owner)
                
                lazy val all =
                    <tr>
                        <td>{closeButton.render}</td>
                        <td>{offer.goingPrice.$}</td>
                        <td>{highBidder map (UserLink(_)) getOrElse <span>-</span>}</td>
                        <td>{offer.expires toNearbyString}</td>
                        <td>{offer.derivative toHumanString}</td>
                    </tr>
                
                lazy val closeButton = FormSubmit("Close") {
                    offer.userClose()
                    
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

