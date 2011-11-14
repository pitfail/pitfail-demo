
package model

import org.joda.time.DateTime

trait NewsSchema {
    self: UserSchema with DBMagic =>
    
    implicit val newsEvents = table[NewsEvent]
    
    sealed trait Action
    case object Bought extends Action
    case object Sold extends Action
    case object Offered extends Action
    case object Accepted extends Action
    case object Declined extends Action
    case object Bid extends Action
    case object Closed extends Action
    case object Exercised extends Action
    
    case class NewsEvent(
            id:        Key = nextID,
            when:      DateTime,
            action:    Action,
            subject:   Link[User],
            recipient: Link[User],
            ticker:    String,
            shares:    Shares,
            dollars:   Dollars,
            price:     Price
        )
        extends KL
        
    // Actually adding events
    
    def addBuyEvent(
            buyer: User, ticker: String, shares: Shares, price: Price)
        : Transaction[NewsEvent] =
    {
        NewsEvent(
            when    = new DateTime,
            action  = Bought,
            subject = buyer,
            ticker  = ticker,
            shares  = shares,
            dollars = shares * price,
            price   = price,
            recipient = ""
        ).insert
    }

    def recentEvents(n: Int) = newsEvents sortBy (_.when.getMillis) take n toList
}


