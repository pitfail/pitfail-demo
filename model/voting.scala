
package model

trait VotingSchema {
    self: DBMagic with UserSchema with DerivativeSchema with NewsSchema =>
    
    val setAsideFraction = Scale("0.03")
    
    implicit val derivativeBuyerSetAsides = table[DerivativeBuyerSetAside]
    implicit val derivativeSellerSetAsides = table[DerivativeSellerSetAside]
    implicit val derivativeBuyerVotes = table[DerivativeBuyerVote]
    implicit val derivativeSellerVotes = table[DerivativeSellerVote]
    
    case class DerivativeBuyerSetAside(
            id:         Key = nextID,
            buyer:      Link[Portfolio],
            seller:     Link[Portfolio],
            derivative: Derivative,
            price:      Dollars,
            remaining:  Scale
        )
        extends KL
        
    case class DerivativeSellerSetAside(
            id:         Key = nextID,
            buyer:      Link[Portfolio],
            seller:     Link[Portfolio],
            derivative: Derivative,
            price:      Dollars,
            remaining:  Scale
        )
        extends KL
    
    case class DerivativeBuyerVote(
            id: Key = nextID,
            caster: Link[Portfolio],
            event:  Link[NewsEvent]
        )
        extends KL
        
    case class DerivativeSellerVote(
            id:     Key = nextID,
            caster: Link[Portfolio],
            event:  Link[NewsEvent]
        )
        extends KL
    
    trait PortfolioWithVotes {
        self: Portfolio =>
        
        def userVoteUp(ev: NewsEvent, aside: DerivativeBuyerSetAside) =
            editDB(this.refetch.voteUp(ev, aside.refetch))
        
        def userVoteDown(ev: NewsEvent, aside: DerivativeSellerSetAside) =
            editDB(this.refetch.voteDown(ev, aside.refetch))
        
        private[model] def voteUp(ev: NewsEvent, aside: DerivativeBuyerSetAside) = {
            val take = aside.remaining * Scale("0.5")
            val price = aside.price * take
                
            for {
                _ <- DerivativeBuyerVote(caster=this,  event=ev).insert
                _ <- enterContract(aside.seller, aside.derivative*take, price)
                
                _ <- aside update (a => a copy (remaining=a.remaining-take))
            }
            yield ()
        }
            
        private[model] def voteDown(ev: NewsEvent, aside: DerivativeSellerSetAside) = {
            val take = aside.remaining * Scale("0.5")
            val price = aside.price * take
                
            for {
                _ <- DerivativeSellerVote(caster=this, event=ev).insert
                _ <- aside.buyer.enterContract(this, aside.derivative*take, price)
                
                _ <- aside update (a => a copy (remaining=a.remaining-take))
            }
            yield ()
        }
        
        private[model] def setupSetAside
                (buyer: Portfolio, seller: Portfolio, deriv: Derivative, price: Dollars) =
        {
            for {
                buyerSetAside <- DerivativeBuyerSetAside(buyer=buyer, seller=seller,
                    derivative=deriv, price=price, remaining=setAsideFraction).insert
                sellerSetAside <- DerivativeSellerSetAside(buyer=buyer, seller=seller,
                    derivative=deriv, price=price, remaining=setAsideFraction).insert
            }
            yield (buyerSetAside, sellerSetAside)
        }
    }
    
    trait NewsEventWithVotes {
        self: NewsEvent =>
            
        def isVotable = action match {
            case Accepted(_, _, _, _, b, s) => true
            case Won(_, _, _, b, s)         => true
            case _  => false
        }
            
        def asVotable: Option[(DerivativeBuyerSetAside,DerivativeSellerSetAside)] =
            action match {
                case Accepted(_, _, _, _, b, s) => Some((b, s))
                case Won(_, _, _, b, s)         => Some((b, s))
                case _  => None
            }
        
        def buyerVotes  = derivativeBuyerVotes  filter (_.event ~~ this)
        def sellerVotes = derivativeSellerVotes filter (_.event ~~ this)
        
        def buyerTally = buyerVotes.length
        def sellerTally = sellerVotes.length
    }
}

