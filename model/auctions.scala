
package model

import org.joda.time.DateTime

trait AuctionSchema {
    schema: UserSchema with DerivativeSchema with SchemaErrors with DBMagic =>
    
    implicit val auctionOffers = table[AuctionOffer]
    implicit val auctionBids   = table[AuctionBid]
    
    // Model tables
    
    case class AuctionOffer(
            id:         Key = nextID,
            derivative: Derivative,
            offerer:    Link[Portfolio],
            price:      Dollars,
            when:       DateTime,
            expires:    DateTime
        )
        extends KL
        with AuctionOfferOps
        
    case class AuctionBid(
            id:    Key = nextID,
            offer: Link[AuctionOffer],
            by:    Link[Portfolio],
            price: Dollars
        )
        extends KL

    trait AuctionOfferOps {
        self: AuctionOffer =>
        
        def bids: Seq[AuctionBid] = sys.error("Not implemented")
        
        def goingPrice =
            if (bids isEmpty) self.price
            else bids map (_.price) max
        
        def highBid: Option[AuctionBid] = sys.error("Not implemented")
        
        def close(): Transaction[Unit] = sys.error("Not implemented")
    }
    
    object AuctionOffer {
        def byID(id: Key) = auctionOffers lookup id getOrElse (
            throw NoSuchAuction
        )
    }
    
    trait PortfolioWithAuctions {
        self: Portfolio =>
        
        def castBid(auction: AuctionOffer, price: Dollars) {
            sys.error("Not implemented")
        }
        
        def auctionOffers: Seq[AuctionOffer] = schema.auctionOffers filter
            (_.offerer == this) toList
    }
    
    def recentAuctions(n: Int): Seq[AuctionOffer] =
        auctionOffers sortBy (_.when.getMillis) take n toList
}

