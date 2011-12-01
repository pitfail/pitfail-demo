
package model

import org.joda.time.DateTime

trait NewsSchema {
    self: DBMagic with SchemaErrors
        with UserSchema with AuctionSchema with StockSchema
        with CommentSchema with VotingSchema =>
    
    implicit val newsEvents = table[NewsEvent]
    
    sealed trait Action {
        protected[model] def report() = reportEvent(this)
    }
    
    case class Bought(buyer: User, stock: String, shares: Shares,
        dollars: Dollars, price: Price) extends Action
    case class Sold(seller: User, stock: String, shares: Shares,
        dollars: Dollars, price: Price) extends Action
    case class Offered(from: User, to: User, derivative: Derivative, price: Dollars) extends Action
    case class Accepted(from: User, to: User, derivative: Derivative, price: Dollars,
        buyerAside: Link[DerivativeBuyerSetAside], sellerAside: Link[DerivativeSellerSetAside]) extends Action
    case class Declined(from: User, to: User, derivative: Derivative, price: Dollars) extends Action
    case class Auctioned(from: User, derivative: Derivative, price: Dollars) extends Action
    case class Bid(from: User, on: AuctionOffer, price: Dollars) extends Action
    case class Won(buyer: User, seller: User, derivative: Derivative,
        buyerAside: Link[DerivativeBuyerSetAside], sellerAside: Link[DerivativeSellerSetAside]) extends Action
    case class Closed(offerer: User, offer: AuctionOffer) extends Action
    case class Exercised(user: User, derivative: Derivative) extends Action
    
    case class NewsEvent(
            id:     Key = nextID,
            when:   DateTime,
            action: Action
        )
        extends KL
        with NewsEventWithComments
        with NewsEventWithVotes
    
    object NewsEvent {
        def byID(id: Key) = newsEvents lookup id getOrElse (throw NoSuchEvent)
    }
        
    // Get the n most recent events!
    def recentEvents(n: Int) = newsEvents sortBy (- _.when.getMillis) take n toList
        
    // Actually adding events
    
    protected[model] def reportEvent(action: Action) =
        NewsEvent(when=new DateTime, action=action).insert
}


