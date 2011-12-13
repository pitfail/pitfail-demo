
package model

// Joda time
import org.joda.time.DateTime
import formats._
import spser._
import scalaz.Scalaz._

import net.liftweb.common.Loggable

trait DerivativeSchema extends Schema {
    schema: UserSchema with DBMagic with NewsSchema
        with AuctionSchema with SchemaErrors with VotingSchema with StockSchema =>
    
    implicit val daCon = DerivativeAsset.apply _
    implicit val dlCon = DerivativeLiability.apply _
    implicit val doCon = DerivativeOffer.apply _
    
    implicit val derivativeAssets: Table[DerivativeAsset] = table[DerivativeAsset]
    implicit val derivativeLiabilities: Table[DerivativeLiability] = table[DerivativeLiability]
    implicit val derivativeOffers: Table[DerivativeOffer] = table[DerivativeOffer]
    
    abstract override def tables = ( derivativeAssets :: derivativeLiabilities
        :: derivativeOffers :: super.tables )
    
    // Model tables
    
    // ref_807
    case class DerivativeAsset(
            id:     Key = nextID,
            peer:   Link[DerivativeLiability],
            scale:  Scale,
            owner:  Link[Portfolio],
            hidden: Boolean
        )
        extends KL
        with DerivativeAssetOps
        
    case class DerivativeLiability(
            id:         Key = nextID,
            name:       String,
            derivative: Derivative,
            remaining:  Scale,
            exec:       DateTime,
            owner:      Link[Portfolio],
            hidden:     Boolean
        )
        extends KL
        
    case class DerivativeOffer(
            id:         Key = nextID,
            derivative: Derivative,
            from:       Link[Portfolio],
            to:         Link[Portfolio],
            price:      Dollars,
            expires:    DateTime
        )
        extends KL
    
    // Operations
        
    // 
    def systemCheckForExercise() = editDB {
        val now = new DateTime
        derivativeAssets.toList map { asset =>
            if (asset.peer.exec isBefore now) asset.executeOnSchedule
            else Transaction(())
        } sequence
    }
    
    trait DerivativeAssetOps {
        self: DerivativeAsset =>
                
        def derivative = self.peer.derivative
        
        // When a user wants to exercise a derivative early.
        // Not all derivatives can be exercised early.
        // ref_583
        def userExecuteManually() { editDB(executeManually) }
        // ref_289
        def systemExecuteOnSchedule() { editDB(executeOnSchedule) }
            
        // ref_319
        def spotValue: Dollars = derivative.spotValue * scale
        
        private[model] def executeManually =
            if (derivative.early) execute
            else throw NotExecutable
            
        // First we must check the condition. Luckily that seems
        // to have survived the model overhaul
        private[model] def executeOnSchedule = 
            if (peer.derivative.condition.isTrue) execute
            else cleanup
        
        private[model] def cleanup = {
            val peer: DerivativeLiability = this.peer
            
            for {
                _ <- this.delete
                _ <- {
                    if (peer.remaining <= scale) peer.delete
                    else peer update (l => l copy (remaining=l.remaining-scale))
                }
            }
            yield ()
        }
            
        private[model] def execute = {
            // This part is really hellish
            val peer: DerivativeLiability = this.peer
            val deriv = peer.derivative * this.scale
            
            for {
                _ <- {
                    if (deriv.condition.isTrue) actuallyExecute(deriv)
                    else Transaction(())
                }
                _ <- cleanup
            }
            yield ()
        }
        
        // ref_519
        private[model] def actuallyExecute(deriv: Derivative) = {
            val seller: Portfolio = peer.owner
            
            val dollars = deriv.securities collect {
                case SecDollar(dollars) => dollars
            } summ
            
            val stocks = deriv.securities collect {
                case x: SecStock => x
            }
            
            val derivatives = deriv.securities collect {
                case x: SecDerivative => x
            }
            
            for {
                _ <- owner.transferCash(seller, dollars)
                _ <- (stocks map (s => owner.transferStock(seller, s.ticker, s.shares))).sequence
                _ <- (derivatives map (s => owner.transferDerivative(seller, s.name, s.scale))).sequence
            }
            yield ()
        }
    }
    
    trait ExerciseOfDoom {
        self: Portfolio with PortfolioWithDerivatives =>
            
        // --------------------------------------------------
        // The direction switches
            
        // ref_392
        private[model] def transferCash(seller: Portfolio, dollars: Dollars) = {
            logger.info("Transfering %s" format dollars)
            if (dollars.isNegative) seller.takeCash(this, -dollars)
            else takeCash(seller, dollars)
        }
        
        // ref_411
        private[model] def transferStock(seller: Portfolio, ticker: String, shares: Shares) = {
            if (shares.isNegative) seller.takeStock(this, ticker, -shares)
            else takeStock(seller, ticker, shares)
        }
        
        private[model] def transferDerivative(seller: Portfolio, name: String, scale: Scale) = {
            if (scale.isNegative) seller.takeDerivative(this, name, -scale)
            else takeDerivative(seller, name, scale)
        }
        
        // --------------------------------------------------
        // Moving stuff around
        
        private[model] def takeCash(seller: Portfolio, dollars: Dollars) = {
            // TODO: This is obviously wrong.
            val actual = if (dollars > seller.cash) seller.cash else dollars
            
            for {
                _ <- this   update (p => p copy (cash=p.cash+actual))
                _ <- seller update (p => p copy (cash=p.cash-actual))
            }
            yield ()
        }
        
        private[model] def takeStock(seller: Portfolio, ticker: String, shares: Shares) = {
            val have = seller.howManyShares(ticker)
            val remaining = shares - have
            
            val price = Stocks.lastTradePrice(ticker)
            val premium = price * Scale("1.15")
            
            // Hack
            val stillRemaining = editDB {
                seller.buyAsMuchAsYouCan(ticker, remaining, premium)
            }
            // Pay the rest in cash
            val cashToMove = stillRemaining * premium
            
            for {
                _ <- takeCash(seller, cashToMove)
                _ <- creditShares(ticker, shares-stillRemaining)
                _ <- seller.debitShares(ticker, shares-stillRemaining)
            }
            yield ()
        }
        
        private[model] def takeDerivative(seller: Portfolio, name: String, scale: Scale) = {
            // Until the user can actually create derivatives that refer to derivatives,
            // I'm not implementing this
            sys.error("Not implemented"): Transaction[Unit]
        }
    }
    
    object DerivativeLiability {
        def byName(name: String) = ( (derivativeLiabilities where ('name ~=~ name)).headOption
            getOrElse(throw NoSuchDerivativeLiability) )
    }
    
    // ref_789
    trait PortfolioWithDerivatives extends ExerciseOfDoom {
        self: Portfolio =>
        
        // ref_74
        def myDerivativeAssets = derivativeAssets where ('owner ~=~ this) toList
        // ref_484
        def myDerivativeLiabilities = derivativeLiabilities where ('owner ~=~ this) toList
        // ref_462
        def myDerivativeOffers = derivativeOffers where ('to ~=~ this) toList
        
        // ref_6
        def userOfferDerivativeTo(recip: Portfolio, deriv: Derivative, price: Dollars) =
            editDB {
                for {
                    offer <- DerivativeOffer(derivative=deriv, from=this,
                        to=recip, price=price, expires=new DateTime).insert
                    _ <- Offered(this, recip, deriv, price).report
                }
                yield offer
            }
        
        // ref_674
        def userOfferDerivativeAtAuction(deriv: Derivative, price: Dollars, expires: DateTime) =
            editDB {
                for {
                    _ <- AuctionOffer(derivative=deriv, offerer=this,
                        price=price, when=new DateTime, expires=expires).insert
                    _ <- Auctioned(this, deriv, price).report
                }
                yield ()
            }
        
        // ref_699
        def userAcceptOffer(id: String) = editDB {
            val offer = derivativeOffers lookup id getOrElse (throw NoSuchOffer)
            for {
                (buyerAside, sellerAside) <- enterContractWithVotes(offer.from, offer.derivative, offer.price)
                _ <- offer.delete
                event <- Accepted(offer.from, offer.to, offer.derivative,
                        price=offer.price, buyerAside=buyerAside, sellerAside=sellerAside).report
                offer.delete
            }
            yield event
        }
        
        // ref_650
        def userDeclineOffer(id: String) = editDB {
            val offer = derivativeOffers lookup id getOrElse (throw NoSuchOffer)
            for {
                _ <- offer.delete
                _ <- Declined(offer.from, offer.to, offer.derivative,
                        price=offer.price).report
            }
            yield ()
        }
        
        private[model] def enterContract
            (seller: Portfolio, deriv: Derivative, price: Dollars, hidden: Boolean=false) =
        {
            for {
                liab <- DerivativeLiability(name=nextID, derivative=deriv, remaining=Scale("1.0"),
                        exec=deriv.exec, owner=seller, hidden=hidden).insert
                
                asset <- DerivativeAsset(peer=liab, scale=Scale("1.0"), owner=this, hidden=hidden).insert
                
                _ <- this update (t => t copy (cash=t.cash-price))
                _ <- seller update (t => t copy (cash=t.cash+price))
            }
            yield {
                // Because of our sneaky monad this check can be done last!
                if (cash < price) throw NotEnoughCash(have=cash, need=price)
            }
        }
        
        private[model] def enterContractWithVotes(seller: Portfolio, deriv: Derivative, price: Dollars) =
            for {
                _ <- enterContract(seller, deriv, price)
                (buyerAside, sellerAside) <- setupSetAside(this, seller, deriv, price)
            }
            yield (buyerAside, sellerAside)
    }
    
    implicit def sqlDerivative: SQLType[Derivative] = new StringType[Derivative] {
        import java.io._
        import org.apache.commons.codec.binary.Base64
        
        def sencode(d: Derivative) = {
            val bo = new ByteArrayOutputStream
            val oo = new ObjectOutputStream(bo)
            oo.writeObject(d)
            oo.close()
            Base64.encodeBase64String(bo.toByteArray)
        }
        def sdecode(s: String) = {
            val bi = new ByteArrayInputStream(Base64.decodeBase64(s))
            val oi = new ObjectInputStream(bi)
            oi.readObject.asInstanceOf[Derivative]
        }
    }
}

// --------------------------------------------------------------------
// The data types

case class Derivative(
    securities: List[Security],
    exec:       DateTime,
    condition:  Condition,
    early:      Boolean
) {
    def *(scale: Scale): Derivative = this.copy(
        securities = securities map (_ * scale)
    )
    
    def spotValue: Dollars = (securities map (_.spotValue))
        .foldLeft(Dollars(0))(_ + _)
}

// -------------------------------------------

sealed abstract class Security {
    def *(scale: Scale): Security
    
    def spotValue: Dollars
}

case class SecDollar(
        amount: Dollars
    ) extends Security
{
    def *(scale: Scale) = SecDollar(amount * scale)
    
    def spotValue = amount
}

case class SecStock(
        ticker: String,
        shares: Shares
    ) extends Security
{
    def *(scale: Scale) = SecStock(ticker, shares*scale)
    
    def spotValue = 
        try Stocks.lastTradePrice(ticker) * shares
        catch {
            // TODO: This is clearly not right
            case _: stockdata.DatabaseException => Dollars(0)
            case _: stockdata.NoSuchStockException => Dollars(0)
        }
}

case class SecDerivative(
        name:  String,
        scale: Scale 
    ) extends Security
{
    import schema._
    
    def *(nextScale: Scale) = SecDerivative(name, scale*nextScale)
    
    def spotValue =
        try readDB {
            (DerivativeLiability byName name).derivative.spotValue * scale
        }
        catch { case NoSuchDerivativeLiability => Dollars(0) }
}

// -------------------------------------------

sealed abstract class Condition {
    def isTrue: Boolean
}

case class CondAlways() extends Condition {
    def isTrue = true
}

case class CondGreater(
        a: ComparableSecurity,
        b: ComparableSecurity
    )
    extends Condition
{
    def isTrue = a.toPrice > b.toPrice
}

// -------------------------------------------

sealed abstract class ComparableSecurity {
    def toPrice: Price
}

case class CompSecStock(
        ticker: String
    ) extends ComparableSecurity
{
    def toPrice = Stocks.lastTradePrice(ticker)
}

case class CompSecDollar(
        amount: Price
    ) extends ComparableSecurity
{
    def toPrice = amount
}

