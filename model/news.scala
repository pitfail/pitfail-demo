
package model

import org.joda.time.DateTime
import spser._
import scala.collection.mutable.ArrayBuffer

trait NewsSchema extends Schema {
    self: DBMagic with SchemaErrors
        with UserSchema with AuctionSchema with StockSchema
        with CommentSchema with VotingSchema =>
    
    val newsEvents: ArrayBuffer[NewsEvent] = ArrayBuffer[NewsEvent]()
    object newsHub extends RefreshHub
    
    sealed trait Action {
        protected[model] def report() = reportEvent(this)
    }
    
    case class Bought(buyer: Link[Portfolio], stock: String, shares: Shares,
        dollars: Dollars, price: Price) extends Action
    case class Sold(seller: Link[Portfolio], stock: String, shares: Shares,
        dollars: Dollars, price: Price) extends Action
    case class Offered(from: Link[Portfolio], to: Link[Portfolio], derivative: Derivative, price: Dollars) extends Action
    case class Accepted(from: Link[Portfolio], to: Link[Portfolio], derivative: Derivative, price: Dollars,
        buyerAside: Link[DerivativeBuyerSetAside], sellerAside: Link[DerivativeSellerSetAside]) extends Action
    case class Declined(from: Link[Portfolio], to: Link[Portfolio], derivative: Derivative, price: Dollars) extends Action
    case class Auctioned(from: Link[Portfolio], derivative: Derivative, price: Dollars) extends Action
    case class Bid(from: Link[Portfolio], on: Link[AuctionOffer], price: Dollars) extends Action
    case class Won(buyer: Link[Portfolio], seller: Link[Portfolio], derivative: Derivative,
        buyerAside: Link[DerivativeBuyerSetAside], sellerAside: Link[DerivativeSellerSetAside]) extends Action
    case class BuyOrdered(buyer: Link[Portfolio], stock: String, shares: Shares, limit: Price) extends Action
    case class SellOrdered(seller: Link[Portfolio], stock: String, shares: Shares, limit: Price) extends Action
    case class Closed(offerer: Link[Portfolio], offer: Link[AuctionOffer]) extends Action
    case class Exercised(user: Link[Portfolio], derivative: Derivative) extends Action
    
    case class NewsEvent(
            id:     Key = nextID,
            when:   DateTime,
            action: Action
        )
        extends KL
        with NewsEventWithComments
        with NewsEventWithVotes
    
    object NewsEvent {
        def byID(id: Key) = (newsEvents filter (_.id==id)).headOption getOrElse (throw NoSuchEvent)
    }
        
    // Get the n most recent events!
    def recentEvents(n: Int) = newsEvents.toList sortBy (- _.when.getMillis) take n toList
        
    // Actually adding events
    
    protected[model] def reportEvent(action: Action) = {
        val event = NewsEvent(when=new DateTime, action=action)
        Transaction(event, Seq(new EditOp {
            def perform() {
                newsEvents += event
                newsHub ! Refresh
            }
            val affectedTables = Nil
        }))
    }
}


