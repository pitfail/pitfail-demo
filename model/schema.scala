
package model

import java.sql.Timestamp
import derivatives._

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl._
import org.squeryl.dsl.ast._

object schema 
    extends H2Schema // backend driver
    with DBMagic     // Updates, selects, etc
    with UserSchema
    with StockSchema
    with DerivativeSchema
    with AuctionSchema
    with NewsSchema

trait UserSchema extends DBMagic {
    case object NoSuchUser extends Exception
    
    implicit val users = table[User]
    implicit val portfolios = table[Portfolio]
    
    case class User(
            id:            Key,
            username:      String,
            mainPortfolio: Link[Portfolio]
        )
        extends KL
    
    case class Portfolio(
            id:    Key,
            cash:  Dollars,
            owner: Link[User],
            loan:  Dollars
        )
        extends KL
    
    object User {
        def ensure(name: String) = byName(name) orCreate {
            sys.error("This is not implemented")
        }
        
        def byName(name: String) = {
            val u = from(users)(u =>
                    where(u.username === name)
                    select(u)
                ) headOption
            
            u getOrElse (throw NoSuchUser)
        }
    }
}

trait StockSchema extends UserSchema {
    implicit val stockAssets = table[StockAsset]
    
    case class StockAsset(
            id:     Key = nextID,
            ticker: String,
            shares: Shares,
            owner:  Link[Portfolio]
        )
        extends KL
}

trait DerivativeSchema extends UserSchema {
    implicit val derivativeAssets      = table[DerivativeAsset]
    implicit val derivativeLiabilities = table[DerivativeLiability]
    implicit val derivativeOffers      = table[DerivativeOffer]
    
    case class DerivativeAsset(
            id:    Key = nextID,
            peer:  Link[DerivativeLiability],
            scale: Scale,
            owner: Link[Portfolio]
        )
        extends KL
        
    case class DerivativeLiability(
            id:         Key = nextID,
            name:       String,
            derivative: Derivative,
            remaining:  Scale,
            exec:       Timestamp,
            owner:      Link[Portfolio]
        )
        extends KL
        
    case class DerivativeOffer(
            id:         Key = nextID,
            derivative: Derivative,
            from:       Link[Portfolio],
            to:         Link[Portfolio],
            price:      Dollars,
            expires:    Timestamp
        )
        extends KL
}

trait AuctionSchema
    extends UserSchema
    with DerivativeSchema
{
    implicit val auctionOffer = table[AuctionOffer]
    implicit val auctionBids  = table[AuctionBid]
    
    case class AuctionOffer(
            id:         Key = nextID,
            derivative: Derivative,
            offerer:    Link[Portfolio],
            price:      Dollars,
            when:       Timestamp,
            expires:    Timestamp
        )
        extends KL
        
    case class AuctionBid(
            id:    Key = nextID,
            offer: Link[AuctionOffer],
            by:    Link[Portfolio],
            price: Dollars
        )
        extends KL
}

trait NewsSchema extends UserSchema {
    implicit val newsEvents = table[NewsEvent]
    
    case class NewsEvent(
            id:        Key = nextID,
            when:      Timestamp,
            action:    String,
            subject:   Link[User],
            recipient: Link[User],
            ticker:    String,
            shares:    Shares,
            dollars:   Dollars,
            price:     Price
        )
        extends KL
}

