
package model

trait VotingSchema {
    self: DBMagic with UserSchema with DerivativeSchema with NewsSchema =>
    
    val setAsideFraction = Scale("0.03")
    
    implicit val derivativeBuyerSetAsides = table[DerivativeBuyerSetAside]
    implicit val derivativeSellerSetAsides = table[DerivativeSellerSetAside]
    implicit val derivativeBuyerVotes = table[DerivativeBuyerVote]
    implicit val derivativeSellerVotes = table[DerivativeSellerVote]
    
    case class DerivativeBuyerSetAside(
            id:        Key = nextID,
            buyer:     Link[User],
            asset:     Link[DerivativeAsset],
        )
        extends KL
        
    case class DerivativeSellerSetAside(
            id:        Key = nextID,
            seller:    Link[User],
            liability: Link[DerivativeLiability],
        )
        extends KL
    
    case class DerivativeBuyerVote(
            id: Key = nextID,
            caster: Link[User],
            event:  Link[NewsEvent]
        )
        extends KL
        
    case class DerivativeSellerVote(
            id:     Key = nextID,
            caster: Link[User],
            event:  Link[NewsEvent]
        )
        extends KL
    
    trait UserWithVotes {
        self: User =>
        
        def userVoteUp(ev: NewsEvent, aside: DerivativeBuyerSetAside) =
            editDB(voteUp(ev, aside))
        
        def userVoteDown(ev: NewsEvent aside: DerivativeSellerSetAside) =
            editDB(voteDown(ev, aside))
        
        private[model] def voteUp(ev: NewsEvent, aside: DerivativeBuyerSetAside) =
            for {
            }
            yield ()
            
        private[model] def voteDown(ev: NewsEvent, aside: DerivativeBuyerSetAside) =
            for {
            }
            yield ()
        
        private[model] def setupSetAside
                (buyer: User, seller: User, deriv: Derivative, price: Dollars) =
        {
            val buyerPort = buyer.votingPortfolio
            val sellerPort = seller.votingPortfolio
            val derivFrac = deriv * setAsideFraction
            
            // We need to change the buyer a little more to cover it
            val actualPrice = price * (Scale(1) + setAsideFraction)
            
            for {
                // Create the set aside asset and liability
                liab <- DerivativeLiability(name=nextID, derivative=derivFrac, remaining=Scale(1),
                    owner=sellerPort, exec=deriv.exec).insert
                asset <- DerivativeAsset(peer=liab, scale=Scale(1), owner=buyerPort).insert
                
                // Create fake "companies" to own them
                buyerSetAside <- DerivativeBuyerSetAside(buyer=buyer, asset=asset).insert
                sellerSetAside <- DerivativeSellerSetAside(seller=seller, liability=liab).insert
            }
            yield (buyerSetAside, sellerSetAside, actualPrice)
        }
    }
}

