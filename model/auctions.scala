
package model

import org.joda.time.DateTime
import scalaz.Scalaz._
import spser._

trait AuctionSchema extends Schema {
    schema: UserSchema with DerivativeSchema
        with SchemaErrors with DBMagic with NewsSchema with VotingSchema =>
    
    implicit val aoCon = AuctionOffer.apply _
    implicit val abCon = AuctionBid.apply _
            
    implicit val auctionOffers: Table[AuctionOffer] = table[AuctionOffer]
    implicit val auctionBids: Table[AuctionBid] = table[AuctionBid]
    
    abstract override def tables = auctionOffers :: auctionBids :: super.tables
    
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
        
    def systemCheckForAuctionClosings() = logger.info("TODO: This")

    trait AuctionOfferOps {
        self: AuctionOffer =>
        
        def bids: Seq[AuctionBid] = auctionBids where ('offer ~=~ this) toList
        
        def goingPrice =
            if (bids isEmpty) self.price
            else bids map (_.price) max
        
        def highBid: Option[AuctionBid] =
            if (bids isEmpty) None
            else Some(bids maxBy (_.price))
        
        def userClose() = editDB {
            val deletion =
                for {
                    _ <- this.delete
                    _ <- (bids map (_.delete)).sequence
                    _ <- Closed(offerer, this).report
                }
                yield ()
            
            highBid match {
                case None      => deletion
                case Some(bid) =>
                    for {
                        _ <- deletion
                        (buyerAside, sellerAside) <-
                            bid.by.enterContractWithVotes(offerer, derivative, bid.price)
                        _ <- Won(bid.by, offerer, derivative, buyerAside, sellerAside).report
                    }
                    yield ()
            }
        }
    }
    
    object AuctionOffer {
        def byID(id: Key) = auctionOffers lookup id getOrElse (
            throw NoSuchAuction
        )
    }
    
    trait PortfolioWithAuctions {
        self: Portfolio =>
        
        def auctionOffers: Seq[AuctionOffer] = schema.auctionOffers where
            ('offerer ~=~ this) toList
            
        def userCastBid(auction: AuctionOffer, price: Dollars) = editDB {
            if (price <= auction.goingPrice)
                throw BidTooSmall(auction.goingPrice)
            
            (
                  AuctionBid(offer=auction, by=this, price=price).insert
                & Bid(this, auction, price).report
            )
        }
    }
    
    def recentAuctions(n: Int): Seq[AuctionOffer] =
        auctionOffers.toList sortBy (- _.when.getMillis) take n toList
}

