
// Written by: Owen Healy
// Written by: Michael Koval

package code
package comet

import net.liftweb.{common, http, util}
import common._
import util._
import http._
import scala.xml._

import intform._
import formats._
import snippet.{PortfolioLink}
import model.schema._

class OutgoingOffers extends Refreshable with Loggable {
    
    def registerWith = auctionOffers
    
    def render = readDB {
        import control.LoginManager._
        import control.PortfolioSwitcher._
        
        try {
            val port   = currentPortfolio
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
                val highBidder = high map (_.by)
                
                lazy val all =
                    <tr>
                        <td>{closeButton.render}</td>
                        <td>{offer.goingPrice.$}</td>
                        <td>{highBidder map (PortfolioLink(_)) getOrElse <span>-</span>}</td>
                        <td>{offer.expires toNearbyString}</td>
                        <td>{offer.derivative toHumanString}</td>
                    </tr>
                
                lazy val closeButton = FormSubmit("Close") {
                    offer.userClose()
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

