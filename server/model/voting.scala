
// Written by: Owen Healy

package model
import spser._

trait VotingSchema extends Schema {
    self: DBMagic with UserSchema with DerivativeSchema with NewsSchema =>
    
    val setAsideFraction = Scale("0.03")
    
    implicit val dbsaCon = DerivativeBuyerSetAside.apply _
    implicit val dssaCon = DerivativeSellerSetAside.apply _
    implicit val dbvCon = DerivativeBuyerVote.apply _
    implicit val dsvCon = DerivativeSellerVote.apply _
    
    implicit val derivativeBuyerSetAsides: Table[DerivativeBuyerSetAside] = table[DerivativeBuyerSetAside]
    implicit val derivativeSellerSetAsides: Table[DerivativeSellerSetAside] = table[DerivativeSellerSetAside]
    implicit val derivativeBuyerVotes: Table[DerivativeBuyerVote] = table[DerivativeBuyerVote]
    implicit val derivativeSellerVotes: Table[DerivativeSellerVote] = table[DerivativeSellerVote]
    
    abstract override def tables = (derivativeBuyerSetAsides :: derivativeSellerSetAsides ::
        derivativeBuyerVotes :: derivativeSellerVotes :: super.tables)
    
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
    
    def highestVotedEvents(n: Int): List[NewsEvent] = (newsEvents.toList filter (_.isVotable)
        sortBy (- _.score) take n)
    
    trait PortfolioWithVotes {
        self: Portfolio =>
        
        // ref_805
        def userVoteUp(ev: NewsEvent, aside: DerivativeBuyerSetAside) =
            editDB(this.refetch.voteUp(ev, aside.refetch))
        
        // ref_940
        def userVoteDown(ev: NewsEvent, aside: DerivativeSellerSetAside) =
            editDB(this.refetch.voteDown(ev, aside.refetch))
        
        private[model] def voteUp(ev: NewsEvent, aside: DerivativeBuyerSetAside) = {
            val take = aside.remaining * Scale("0.5")
            val price = aside.price * take
                
            for {
                _ <- DerivativeBuyerVote(caster=this,  event=ev).insert
                _ <- enterContract(aside.seller, aside.derivative*take, price, hidden=true)
                
                _ <- aside update (a => a copy (remaining=a.remaining-take))
            }
            yield ()
        }
            
        private[model] def voteDown(ev: NewsEvent, aside: DerivativeSellerSetAside) = {
            val take = aside.remaining * Scale("0.5")
            val price = aside.price * take
                
            for {
                _ <- DerivativeSellerVote(caster=this, event=ev).insert
                _ <- aside.buyer.enterContract(this, aside.derivative*take, price, hidden=true)
                
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
                case Accepted(_, _, _, _, b, s) => Some((b.extract, s.extract))
                case Won(_, _, _, b, s)         => Some((b.extract, s.extract))
                case _  => None
            }
        
        // ref_146
        def buyerVotes  = derivativeBuyerVotes  where ('event ~=~ this) toList
        // ref_405
        def sellerVotes = derivativeSellerVotes where ('event ~=~ this) toList
        
        def buyerTally = buyerVotes.length
        def sellerTally = sellerVotes.length
        
        def score = buyerTally - sellerTally
    }
}

