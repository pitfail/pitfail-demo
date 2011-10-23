
package code
package model

import scala.math.{BigDecimal}

import org.squeryl
import squeryl.PrimitiveTypeMode._
import squeryl.annotations.Column
import squeryl.{KeyedEntity,
    SessionFactory, Session, Table}
import squeryl.customtypes.{LongField}
import squeryl.adapters.H2Adapter
import squeryl.dsl._
import squeryl.dsl.ast._
import links._
import java.sql.Timestamp
import java.util.UUID

import Stocks.{StockShares, stockPrice}
import derivatives._
import net.liftweb.common.Loggable

object Schema extends squeryl.Schema {
    
    type Dollars = BigDecimal
    
    implicit val users                 = table[User]
    implicit val portfolios            = table[Portfolio]
    implicit val stockAssets           = table[StockAsset]
    implicit val newsEvents            = table[NewsEvent]
    implicit val derivativeAssets      = table[DerivativeAsset]
    implicit val derivativeLiabilities = table[DerivativeLiability]
    implicit val derivativeOffers      = table[DerivativeOffer]
    
    // Errors that can occur during model operations
    case object NegativeVolume extends Exception
    case class NotEnoughCash(have: Dollars, need: Dollars) extends Exception
    case class DontOwnStock(ticker: String) extends Exception
    case class NotEnoughShares(have: BigDecimal, need: BigDecimal) extends Exception
    case object OfferExpired extends Exception
    case object NotExecutable extends Exception
    
    def trans[A](x: =>A) = inTransaction(x)
    
    def byUsername(name: String): Option[User] =
        trans {
            from(users) (u =>
                where(u.username === name)
                select(u)
            ) headOption
        }
    
    def ensureUser(username: String): User =
        trans {
            byUsername(username) match {
                case None =>
                    val user = User(username = username)
                    users insert user
                    
                    val port = Portfolio(
                        owner = user,
                        cash  = BigDecimal("2000.0")
                    )
                    portfolios insert port
                    
                    user.mainPortfolio = port
                    user.update()
                    
                    user
                    
                case Some(user) => user
            }
        }
    
    def eventsSinceDays(n: Int): Seq[NewsEvent] = {
        val msecs = System.currentTimeMillis - (n: Long) * 24*60*60
        val stamp = new Timestamp(msecs)
        
        trans {
            from(newsEvents)(e =>
                where(e.when > stamp)
                select(e)
            )
            .toList
        }
    }
    
    def recentEvents(n: Int): Seq[NewsEvent] = trans {
        from(newsEvents)(e =>
            select(e)
            orderBy(e.when desc)
        )
        .page(0, n)
        .toList
    }
    
    case class User(
        var id:            Long             = 0,
        var username:      String           = "",
        var mainPortfolio: Link[Portfolio]  = 0
        )
        extends KL
    {
        def offerDerivativeTo(recip: User, deriv: Derivative): Unit = trans {
            // Can anything go wrong here? I'm not aware...
            val offer = DerivativeOffer(
                handle = UUID.randomUUID.toString,
                mode   = deriv.serialize,
                from   = this.mainPortfolio,
                to     = recip.mainPortfolio
            )
            
            derivativeOffers.insert(offer)
        }
        
        def offerDerivativeAtAuction(deriv: Derivative): Unit = trans {
            throw new IllegalStateException("Not implemented, sorry ;( ;( ;(")
        }
        
        def acceptOffer(offerID: String) { mainPortfolio.acceptOffer(offerID) }
        
        def declineOffer(offerID: String) { mainPortfolio.declineOffer(offerID) }
    }
    
    case class Portfolio(
        var id:            Long             = 0,
        var cash:          BigDecimal       = 0,
        var owner:         Link[User]       = 0
        )
        extends KL with Loggable
    {
        def buyStock(ticker: String, volume: Dollars)
            = buy(StockShares(ticker, volume))
        
        def buy(stock: StockShares): StockAsset = trans {
            if (cash < stock.price)
                throw NotEnoughCash(cash, stock.price)
            if (stock.shares < 0)
                throw NegativeVolume
            
            val asset = stockAsset(stock.ticker)
            
            asset.shares += stock.shares
            cash -= stock.value

            newsEvents insert NewsEvent(
                action  = "buy",
                subject = owner,
                ticker  = stock.ticker,
                shares  = stock.shares,
                price   = stock.price
            )
            this.update()
            asset.update()
            
            asset
        }
        
        def stockAsset(ticker: String): StockAsset = trans {
            // Look to see if we can lump this in with an
            // existing asset
            haveTicker(ticker) match {
                case Some(asset) => asset
                case None        => makeTicker(ticker)
            }
        }
        
        def sell(stock: StockShares): Unit = trans {
            val asset =
                haveTicker(stock.ticker) match {
                    case Some(asset) => asset
                    case None => throw DontOwnStock(stock.ticker)
                }
            
            if (stock.shares > asset.shares)
                throw NotEnoughShares(asset.shares, stock.shares)
            
            cash += stock.value
            asset.shares -= stock.shares
            if (asset.shares <= 0)
                asset.delete()
            else
                asset.update()
            
            newsEvents insert NewsEvent(
                action  = "sell",
                subject = owner,
                ticker  = stock.ticker,
                shares  = stock.shares,
                price   = stock.price
            )
            this.update()
        }
        
        def sellAll(ticker: String): Unit = trans {
            val asset =
                haveTicker(ticker) match {
                    case Some(asset) => asset
                    case None => throw DontOwnStock(ticker)
                }
            
            val stock = StockShares(asset.ticker, asset.shares)
            cash += stock.value
            
            newsEvents insert NewsEvent(
                action  = "sell",
                subject = owner,
                ticker  = stock.ticker,
                shares  = stock.shares,
                price   = stock.price
            )
            asset.delete()
            this.update()
        }
        
        def haveTicker(ticker: String): Option[StockAsset] =
            trans {
                from(stockAssets) (a =>
                    where(
                            (a.ticker === ticker)
                        and (a.portfolio === this)
                    )
                    select(a)
                ) headOption
            }
        
        def makeTicker(ticker: String): StockAsset =
            trans {
                val asset = StockAsset(
                    ticker    = ticker,
                    portfolio = this
                )
                stockAssets.insert(asset)
                asset
            }
        
        def myStockAssets: Seq[StockAsset] =
            trans {
                from(stockAssets) (a =>
                    where(
                        a.portfolio === this
                    )
                    select(a)
                ) toList
            }
        
        def myDerivativeAssets: Seq[DerivativeAsset] =
            trans {
                from(derivativeAssets) (a =>
                    where (
                        a.owner === this
                    )
                    select(a)
                ) toList
            }
        
        def myDerivativeLiabilities: Seq[DerivativeLiability] =
            trans {
                from(derivativeLiabilities) (l =>
                    where (
                        l.owner === this
                    )
                    select(l)
                ) toList
            }
        
        def myOffers: Seq[DerivativeOffer] =
            trans {
                from (derivativeOffers) (o =>
                    where (
                        o.to === this
                    )
                    select(o)
                ) toList
            }
        
        def acceptOffer(offerID: String): Unit = trans {
            val offer = findOffer(offerID)
            val deriv = offer.derivative
            
            val liab =
                DerivativeLiability(
                    mode  = offer.mode,
                    // TODO: This is obviously wrong
                    exec  = now,
                    owner = offer.from
                )
            liab.insert()
            
            val asset =
                DerivativeAsset(
                    peer  = liab,
                    scale = BigDecimal("1.0"),
                    owner = this
                )
            asset.insert()
            offer.delete()
        }
        
        def declineOffer(offerID: String): Unit = trans {
            val offer = findOffer(offerID)
            offer.delete()
        }
        
        def findOffer(offerID: String): DerivativeOffer = trans {
            val offerOption =
                from(derivativeOffers) (o =>
                    where(
                            (o.handle === offerID)
                        and (o.to     === this)
                    )
                    select(o)
                ) headOption
            
            offerOption match {
                case Some(o) => o
                case None    => throw OfferExpired
            }
        }
        
        def take(sec: Security, peer: Portfolio): Unit = trans {
            sec match {
                case SecDollar(amt) =>
                    if (amt > 0) takeCash(amt, peer)
                    else peer.takeCash(amt, this)
                    
                case SecStock(ticker, shares) =>
                    if (shares > 0) takeStock(ticker, shares, peer)
                    else peer.takeStock(ticker, shares, this)
                
                case SecDerivative(name, scale) =>
                    if (scale > 0) takeDerivative(name, scale, peer)
                    else peer.takeDerivative(name, scale, this)
            }
        }
        
        def takeCash(amt: Dollars, peer: Portfolio): Unit = trans {
            cash += peer.loseCash(amt)
            this.update()
        }
        
        def loseCash(amt: Dollars): Dollars = trans {
            val (actual, nextCash) =
                if (amt > cash) (cash, BigDecimal("0"))
                else (amt, cash - amt)
            
            cash = nextCash
            this.update()
            
            actual
        }
        
        def takeStock(ticker: String, shares: BigDecimal, peer: Portfolio): Unit = trans {
            val actual = peer.loseStock(ticker, shares)
            val asset = stockAsset(ticker)
            
            asset.shares += actual
            asset.update()
        }
        
        def loseStock(ticker: String, shares: BigDecimal): BigDecimal = trans {
            var leftover: BigDecimal = shares
            
            haveTicker(ticker) match {
                case None =>
                case Some(asset) =>
                    if (asset.shares > leftover) {
                        asset.shares -= leftover
                        leftover = 0
                        asset.update()
                    }
                    else {
                        leftover -= asset.shares
                        asset.delete()
                    }
            }
            
            if (leftover > 0) {
                val price = stockPrice(ticker)
                val sharesFromCash = cash / price
                if (sharesFromCash > leftover) {
                    cash -= leftover * price
                    leftover = 0
                }
                else {
                    cash = 0
                    leftover -= sharesFromCash
                }
                this.update()
            }
            
            return shares - leftover
        }
        
        def takeDerivative(name: String, scale: BigDecimal, from: Portfolio):
            Unit =
        trans {
            val actuals = from.loseDerivative(name, scale)
            for (
                (liability, scale) <- actuals
            ) {
                val asset = DerivativeAsset(
                    peer  = liability,
                    scale = scale,
                    owner = this
                )
                asset.insert()
            }
        }
        
        def loseDerivative(name: String, scale: BigDecimal):
            Seq[(DerivativeLiability, BigDecimal)] =
        trans {
            val origLiab = DerivativeLiability.byName(name) getOrElse {
                // TODO: We need to do this just a little bit better......
                throw new IllegalStateException("The derivative vanished...")
            }
            val myAsset = DerivativeAsset.byPeer(origLiab, this)
            
            myAsset match {
                case None =>
                    val newLiab = replicateLiability(origLiab, scale)
                    (newLiab, BigDecimal("1.0")) :: Nil
                    
                case Some(myAsset) =>
                    if (myAsset.scale >= scale) {
                        myAsset.scale -= scale
                        myAsset.update()
                        (origLiab, scale) :: Nil
                    }
                    else {
                        val newLiab = replicateLiability(origLiab, scale - myAsset.scale)
                        myAsset.delete()
                        (origLiab, myAsset.scale) :: (newLiab, BigDecimal("1.0")) :: Nil
                    }
            }
        }
        
        def replicateLiability(liab: DerivativeLiability, scale: BigDecimal):
            DerivativeLiability =
        trans {
            val newMode = (liab.derivative * scale).serialize
            val newLiab = DerivativeLiability(
                mode  = newMode,
                exec  = liab.exec,
                owner = this
            )
            newLiab.insert()
            
            newLiab
        }
    }

    case class StockAsset(
        var id:            Long             = 0,
        var ticker:        String           = "",
        var shares:        BigDecimal       = 0,
        var portfolio:     Link[Portfolio]  = 0
        )
        extends KL
    
    case class DerivativeAsset(
        var id:    Long            = 0,
        var peer:  Link[DerivativeLiability] = 0,
        var scale: BigDecimal      = 0,
        var owner: Link[Portfolio] = 0
        )
        extends KL
        with Loggable
    {
        def derivative: Derivative = trans {
            peer.derivative * scale
        }
        
        def execute(): Unit = trans {
            val deriv = derivative
            if (! deriv.early) throw NotExecutable
                
            val secs  = deriv.securities
            
            for (sec <- secs) {
                owner.take(sec, peer.owner)
            }
            
            peer.reduceScale(scale)
            this.delete()
        }
    }
    object DerivativeAsset {
        def byPeer(liab: DerivativeLiability,  owner: Portfolio): Option[DerivativeAsset] =
        trans {
            val allAssets = from(derivativeAssets)(da =>
                where(
                         da.peer  === liab
                    and  da.owner === owner
                )
                select(da)
            ) toList
        
            // TODO: If there are duplicate derivative assets, now is a really good
            // time to clean them up!
            allAssets headOption
        }
    }
    
    case class DerivativeLiability(
        var id:         Long            = 0,
        var name:       String          = UUID.randomUUID.toString.substring(0, 5),
        var mode:       Array[Byte]     = Array(),
        var remaining:  BigDecimal      = BigDecimal("1.0"),
        var exec:       Timestamp       = now,
        var owner:      Link[Portfolio] = 0
        )
        extends KL
    {
        def derivative: Derivative = trans {
            Derivative.deserialize(mode)
        }
        
        def reduceScale(scale: BigDecimal): Unit = trans {
            remaining -= scale
            
            if (remaining <= 0) this.delete()
            else this.update()
        }
    }
    object DerivativeLiability {
        def byName(name: String): Option[DerivativeLiability] = trans {
            from(derivativeLiabilities)(dl =>
                where(
                    dl.name === name
                )
                select(dl)
            ) headOption
        }
    }
    
    case class DerivativeOffer(
        var id:     Long            = 0,
        var handle: String          = "",
        var mode:   Array[Byte]     = Array(),
        @Column("sender")
        var from:   Link[Portfolio] = 0,
        var to:     Link[Portfolio] = 0
        )
        extends KL
    {
        def derivative: Derivative = Derivative.deserialize(mode)
    }
        
    case class NewsEvent(
        var id:        Long       = 0,
        var when:      Timestamp  = now,
        var action:    String     = "",
        var subject:   Link[User] = 0,
        var recipient: Link[User] = 0,
        var ticker:    String     = "",
        var shares:    BigDecimal = 0,
        var price:     BigDecimal = 0
        )
        extends KL
        
    def now: Timestamp = new Timestamp(System.currentTimeMillis)
        
    def init() {
        Class.forName("org.h2.Driver")
        SessionFactory.concreteFactory = Some(() =>
            Session.create(
                java.sql.DriverManager.getConnection("jdbc:h2:data;AUTO_SERVER=TRUE"),
                new H2Adapter
            )
        )
    }
    
    def createSchema() {
        init()
        
        trans {
            this.create
        }
    }
    
    def clearDatabase() {
        new java.io.File("data.h2.db").delete()
        createSchema()
        ensureUser("ellbur_k_a")
    }
}

