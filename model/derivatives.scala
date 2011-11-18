
package model

// Joda time
import org.joda.time.DateTime
import formats._

import net.liftweb.common.Loggable

trait DerivativeSchema {
    schema: UserSchema with DBMagic with NewsSchema with AuctionSchema with SchemaErrors =>
    
    implicit val derivativeAssets      = table[DerivativeAsset]
    implicit val derivativeLiabilities = table[DerivativeLiability]
    implicit val derivativeOffers      = table[DerivativeOffer]
    
    // Model tables
    
    case class DerivativeAsset(
            id:    Key = nextID,
            peer:  Link[DerivativeLiability],
            scale: Scale,
            owner: Link[Portfolio]
        )
        extends KL
        with DerivativeAssetOps
        
    case class DerivativeLiability(
            id:         Key = nextID,
            name:       String,
            derivative: Derivative,
            remaining:  Scale,
            exec:       DateTime,
            owner:      Link[Portfolio]
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
        
    trait DerivativeAssetOps {
        self: DerivativeAsset =>
            
        def derivative = self.peer.derivative
        
        // When a user wants to exercise a derivative early.
        // Not all derivatives can be exercised early.
        def userExecuteManually() { sys.error("Not implemented") }
    }
    
    object DerivativeLiability {
        def byName(name: String) = derivativeLiabilities
            .filter(_.name == name)
            .headOption
            .getOrElse(throw NoSuchDerivativeLiability)
    }
    
    trait PortfolioWithDerivatives {
        self: Portfolio =>
        
        def myDerivativeAssets = schema.derivativeAssets filter (_.owner ~~ this) toList
        def myDerivativeLiabilities = schema.derivativeLiabilities filter (_.owner ~~ this) toList
        def myDerivativeOffers = schema.derivativeOffers filter (_.to ~~ this) toList
        
        def userOfferDerivativeTo(recip: User, deriv: Derivative, price: Dollars) =
            editDB {
                for {
                    _ <- DerivativeOffer(derivative=deriv, from=this,
                        to=recip.mainPortfolio, price=price, expires=new DateTime).insert
                    _ <- Offered(this.owner, recip, deriv, price).report
                }
                yield ()
            }
        
        def userOfferDerivativeAtAuction(deriv: Derivative, price: Dollars, expires: DateTime) =
            editDB {
                for {
                    _ <- AuctionOffer(derivative=deriv, offerer=this,
                        price=price, when=new DateTime, expires=expires).insert
                    _ <- Auctioned(this.owner, deriv, price).report
                }
                yield ()
            }
        
        def userAcceptOffer(id: String) = editDB {
            val offer = derivativeOffers lookup id getOrElse (throw NoSuchOffer)
            for {
                _ <- accept(offer.price, offer.from, offer.derivative)
                _ <- Accepted(offer.from.owner, offer.to.owner, offer.derivative,
                        price=offer.price).report
            }
            yield ()
        }
        
        def userDeclineOffer(id: String) = editDB {
            val offer = derivativeOffers lookup id getOrElse (throw NoSuchOffer)
            for {
                _ <- offer.delete
                _ <- Declined(offer.from.owner, offer.to.owner, offer.derivative,
                        price=offer.price).report
            }
            yield ()
        }
        
        protected[model] def accept(price: Dollars, seller: Portfolio, deriv: Derivative) = {
            if (cash < price) throw NotEnoughCash(cash, price)
            
            for {
                _ <- this update (t => copy (cash=t.cash-price))
                _ <- seller update (t => copy (cash=t.cash+price))
                _ <- enterContract(seller, deriv)
            }
            yield ()
        }
        
        protected[model] def enterContract(seller: Portfolio, deriv: Derivative) =
            for {
                liab <- DerivativeLiability(name=nextID, derivative=deriv, remaining=Scale("1.0"),
                        exec=deriv.exec, owner = seller).insert
                _ <- DerivativeAsset(peer=liab, scale=Scale("1.0"), owner=this).insert
            }
            yield ()
    }
}

// --------------------------------------------------------------------
// The data types

case class Derivative(
    securities: Seq[Security],
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
        try Stocks.stockPrice(ticker) * shares
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

case object CondAlways extends Condition {
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
    def toPrice = Stocks.stockPrice(ticker)
}

case class CompSecDollar(
        amount: Price
    ) extends ComparableSecurity
{
    def toPrice = amount
}

