
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
    
    type P = Portfolio
    type BSA = DerivativeBuyerSetAside
    type SSA = DerivativeSellerSetAside
    
    case class Bought(buyer: P, stock: String, shares: Shares,
        dollars: Dollars, price: Price) extends Action
    case class Sold(seller: P, stock: String, shares: Shares,
        dollars: Dollars, price: Price) extends Action
    case class Offered(from: P, to: P, derivative: Derivative, price: Dollars) extends Action
    case class Accepted(from: P, to: P, derivative: Derivative, price: Dollars,
        buyerAside: Link[DerivativeBuyerSetAside], sellerAside: Link[DerivativeSellerSetAside]) extends Action
    case class Declined(from: P, to: P, derivative: Derivative, price: Dollars) extends Action
    case class Auctioned(from: P, derivative: Derivative, price: Dollars) extends Action
    case class Bid(from: P, on: AuctionOffer, price: Dollars) extends Action
    case class Won(buyer: P, seller: P, derivative: Derivative, buyerAside: Link[BSA],
        sellerAside: Link[SSA]) extends Action
    case class Closed(offerer: P, offer: AuctionOffer) extends Action
    case class Exercised(user: P, derivative: Derivative) extends Action
    
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


