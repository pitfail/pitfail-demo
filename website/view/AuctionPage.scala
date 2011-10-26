
package code
package snippet

import net.liftweb.{common, http, util}
import common._
import util._
import http._
import scala.xml._

import intform._
import model._
import model.Schema._
import formats._

class AuctionPage extends Page with Loggable {
    
    def render = refreshable.render
    val param = S param "id"
    
    val refreshable = Refreshable(
        normal openOr error
    )
        
    def normal: Box[NodeSeq] = trans {
        for {
            idText  <- param
            id       = java.lang.Long parseLong idText
            auction  <-
                try {
                    Full(AuctionOffer.byID(id))
                }
                catch { case NoSuchAuction =>
                    Empty
                }
            
            goingPrice = auction.goingPrice
            seller     = auction.offerer.owner
            deriv      = auction.derivative
        } yield  {
            lazy val all =
                <div id="auction">
                    <h2>Auction #{id}</h2>
                    
                    <table>
                        <tr>
                            <td>Seller:</td>     <td>{UserLink(seller.username)}</td>
                        </tr>
                        <tr>
                            <td>GoingPrice:</td> <td>{goingPrice.$}</td>
                        </tr>
                        <tr>
                            <td>Derivative:</td>
                            <td>
                            {derivative}
                            </td>
                        </tr>
                    </table>
                    
                    <h3>Cast a bid:</h3>
                    {bidForm.render}
                </div>
            
            lazy val derivative =
                <table>
                    <tr>
                        <td>Securities:</td> <td>{deriv.securities toHumanString}</td>
                    </tr>
                    <tr>
                        <td>Exercise Date:</td> <td>{deriv.exec toNearbyString}</td>
                    </tr>
                    <tr>
                        <td>Condition:</td> <td>{deriv.condition toHumanString}</td>
                    </tr>
                </table>
            
            lazy val bidForm: Form[Bid] = Form(Bid,
                (
                    bidField
                ),
                <p>Your bid: {bidField.main} {bidField.errors}
                    (> {goingPrice.$}) {castSubmit.main}
                </p>
                <p>{castSubmit.errors}</p>
            )
            lazy val bidField = DollarsField(goingPrice.no$)
            lazy val castSubmit = Submit(bidForm, "Cast") { case Bid(amt) =>
                import control.LoginManager._
                
                try {
                    val user = currentUser
                    user.mainPortfolio.castBid(auction, amt)
                    
                    bidForm.reset()
                    comet.AuctionThumbnail ! comet.Refresh
                    comet.News ! comet.Refresh
                    comet.OutgoingOffers ! comet.Refresh
                    refreshable.refresh()
                }
                catch {
                    case NotLoggedIn =>
                        throw BadInput("You must be logged in to bid")
                    case BidTooSmall(going) =>
                        throw BadInput("The bid must be more than %s" format going.$)
                }
            }
            
            all
        }
    }
    
    case class Bid(amount: Dollars)
    
    def error = <p>This auction does not seem to exist anymore.</p>
}

