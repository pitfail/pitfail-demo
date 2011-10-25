
package model

import scala.math.{BigDecimal}

import org.squeryl
import squeryl.PrimitiveTypeMode._
import squeryl.annotations.Column
import squeryl.{KeyedEntity,
    SessionFactory, Session, Table}
import squeryl.customtypes.{BigDecimalField,LongField}
import squeryl.adapters.H2Adapter
import squeryl.dsl._
import squeryl.dsl.ast._
import links._
import java.sql.Timestamp
import java.util.UUID

import Stocks.{StockShares, stockPrice}
import derivatives._
import net.liftweb.common.Loggable

object Schema extends squeryl.Schema with Loggable {
    implicit val users                 = table[User]
    implicit val portfolios            = table[Portfolio]
    implicit val stockAssets           = table[StockAsset]
    implicit val newsEvents            = table[NewsEvent]
    implicit val derivativeAssets      = table[DerivativeAsset]
    implicit val derivativeLiabilities = table[DerivativeLiability]
    implicit val derivativeOffers      = table[DerivativeOffer]
    implicit val auctionOffers         = table[AuctionOffer]
    implicit val auctionBids           = table[AuctionBid]
    
    // Errors that can occur during model operations
    case object NegativeVolume extends Exception
    case class NotEnoughCash(have: Dollars, need: Dollars) extends Exception
    case class NotEnoughShares(have: Shares, need: Shares) extends Exception
    case class DontOwnStock(ticker: String) extends Exception
    case object OfferExpired extends Exception
    case object NotExecutable extends Exception
    case object NoSuchAuction extends Exception
    case class BidTooSmall(going: Dollars) extends Exception
    
    case class Dollars(dollars: BigDecimal) extends BigDecimalField(dollars) with Ordered[Dollars] {
        def +(other: Dollars) = Dollars(dollars + other.dollars)
        def -(other: Dollars) = Dollars(dollars - other.dollars)
        def /(shares: Shares) = Price(dollars / shares.shares)
        def /(price: Price)   = Shares(dollars / price.price)
        def *(scale: Scale)   = Dollars(scale.scale * dollars)
        def unary_- = copy(dollars = -dollars)

        def compare(other: Dollars) = dollars.compare(other.dollars)

        def $: String = "$%.2f" format (dollars doubleValue)
    }
    object Dollars {
        def apply(str: String): Dollars = Dollars(BigDecimal(str))
    }

    case class Shares(shares: BigDecimal) extends BigDecimalField(shares) with Ordered[Shares] {
        def +(other: Shares) = Shares(shares + other.shares)
        def -(other: Shares) = Shares(shares - other.shares)
        def *(price: Price)  = Dollars(price.price * shares)
        def *(scale: Scale)   = Shares(scale.scale * shares)
        def unary_- = copy(shares = -shares)
        
        def compare(other: Shares) = shares.compare(other.shares)

        def ###(): String = "%.0f" format (shares doubleValue)
    }
    object Shares {
        def apply(str: String): Shares = Shares(BigDecimal(str))
    }

    case class Price(price: BigDecimal) extends BigDecimalField(price) with Ordered[Price] {
        def +(other: Price)   = Price(price + other.price)
        def -(other: Price)   = Price(price - other.price)
        def *(shares: Shares) = Dollars(shares.shares * price)
        def *(scale: Scale)   = Price(scale.scale * price)

        def compare(other: Price) = price.compare(other.price)

        def $: String = "$%.2f" format (price doubleValue)
    }
    object Price {
        def apply(str: String): Price = Price(BigDecimal(str))
    }

    case class Scale(scale: BigDecimal) extends BigDecimalField(scale) with Ordered[Scale] {
        def +(other: Scale)              = Scale(scale + other.scale)
        def -(other: Scale)              = Scale(scale - other.scale)
        def *(dollars: Dollars): Dollars = Dollars(scale * dollars.dollars)
        def *(price: Price): Price       = Price(scale * price.price)
        def *(shares: Shares): Shares    = Shares(scale * shares.shares)
        def *(other: Scale): Scale       = Scale(scale * other.scale)

        def compare(other: Scale) = scale.compare(other.scale)

        def %(): String = "%.0f%%" format ((scale doubleValue) * 100)
    }
    object Scale {
        def apply(str: String): Scale = Scale(str)
    }

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
                    
                    // TODO: Starting cash should be moved to a properties file.
                    val port = Portfolio(
                        owner = user,
                        cash  = Dollars("2000.0")
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
    
    // Check for derivatives that can be exercised
    def checkForExercise(): Unit = trans {
        val overdue =
            from(derivativeLiabilities)(d =>
                where(d.exec <= now)
                select(d)
                orderBy(d.exec)
            )
        
        overdue foreach { dl =>
            val assets = from(derivativeAssets)(d =>
                where(d.peer === dl)
                select(d)
            )
            val deriv = dl.derivative
            
            if (deriv.condition.isTrue) {
                assets foreach (_.executeOnSchedule())
            }
            else {
                assets foreach (_.delete())
            }
            
            dl.delete()
        }
    }
    
    def recentAuctions(n: Int): Seq[AuctionOffer] = trans {
        from(auctionOffers)(o =>
            select(o)
            orderBy(o.when desc)
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
        def offerDerivativeTo(
            recip: User,
            deriv: Derivative,
            price: Dollars
        ): Unit = trans {
            val offer = DerivativeOffer(
                handle = UUID.randomUUID.toString,
                mode   = deriv.serialize,
                from   = this.mainPortfolio,
                to     = recip.mainPortfolio,
                price  = price
            )
            
            offer.insert()
        }
        
        def offerDerivativeAtAuction(
            deriv: Derivative,
            price: Dollars
        ): Unit = trans {
            val offer = AuctionOffer(
                mode     = deriv.serialize,
                offerer  = this.mainPortfolio,
                price    = price
            )
            
            offer.insert()
        }
        
        def acceptOffer(offerID: String) { mainPortfolio.acceptOffer(offerID) }
        
        def declineOffer(offerID: String) { mainPortfolio.declineOffer(offerID) }
    }
    
    case class Portfolio(
        var id:            Long             = 0,
        var cash:          Dollars          = Dollars(0),
        var owner:         Link[User]       = 0
        )
        extends KL with Loggable
    {
        def buyStock(ticker: String, dollars: Dollars)
            = buy(StockShares(ticker, dollars))
        
        def buy(stock: StockShares): StockAsset = trans {
            if (cash < stock.value)
                throw NotEnoughCash(cash, stock.value)
            if (stock.shares < Shares(0))
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
        
        def sellStock(ticker: String, dollars: Dollars): Unit = trans {
            val asset =
                haveTicker(ticker) match {
                    case Some(asset) => asset
                    case None => throw DontOwnStock(ticker)
                }
            val shares = dollars / stockPrice(ticker)
            
            if (shares > asset.shares)
                throw NotEnoughShares(asset.shares, shares)
            
            cash += dollars
            asset.shares -= shares
            if (asset.shares <= Shares(0))
                asset.delete()
            else
                asset.update()
            
            newsEvents insert NewsEvent(
                action  = "sell",
                subject = owner,
                ticker  = ticker,
                shares  = shares,
                price   = stockPrice(ticker)
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
                    exec  = new Timestamp(deriv.exec.getMillis),
                    owner = offer.from
                )
            liab.insert()
            
            val asset =
                DerivativeAsset(
                    peer  = liab,
                    scale = Scale("1.0"),
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
                    if (amt > Dollars(0)) takeCash(amt, peer)
                    else peer.takeCash(amt, this)
                    
                case SecStock(ticker, shares) =>
                    if (shares > Shares(0)) takeStock(ticker, shares, peer)
                    else peer.takeStock(ticker, shares, this)
                
                case SecDerivative(name, scale) =>
                    if (scale > Scale(0)) takeDerivative(name, scale, peer)
                    else peer.takeDerivative(name, scale, this)
            }
        }
        
        def takeCash(amt: Dollars, peer: Portfolio): Unit = trans {
            val actual = peer.loseCash(amt)
            
            for (me <- this.refetch) {
                me.cash += actual
                me.update()
            }
        }
        
        def loseCash(amt: Dollars): Dollars = trans {
            val (actual, nextCash) =
                if (amt > cash) (cash, Dollars("0"))
                else (amt, cash - amt)
            
            cash = nextCash
            this.update()
            
            actual
        }
        
        def takeStock(ticker: String, shares: Shares, peer: Portfolio): Unit = trans {
            val actual = peer.loseStock(ticker, shares)
            val asset = stockAsset(ticker)
            
            asset.shares += actual
            asset.update()
        }
        
        def loseStock(ticker: String, shares: Shares): Shares = trans {
            var leftover: Shares = shares
            
            haveTicker(ticker) match {
                case None =>
                case Some(asset) =>
                    if (asset.shares > leftover) {
                        asset.shares -= leftover
                        leftover = Shares(0)
                        asset.update()
                    }
                    else {
                        leftover -= asset.shares
                        asset.delete()
                    }
            }
            
            if (leftover > Shares(0)) {
                val price = stockPrice(ticker)
                val sharesFromCash = cash / price
                if (sharesFromCash > leftover) {
                    cash -= leftover * price
                    leftover = Shares(0)
                }
                else {
                    cash = Dollars(0)
                    leftover -= sharesFromCash
                }
                this.update()
            }
            
            return shares - leftover
        }
        
        def takeDerivative(name: String, scale: Scale, from: Portfolio):
            Unit =
        trans {
            val actuals = from.loseDerivative(name, scale)
            for (
                (liability, scale) <- actuals
            ) {
                DerivativeAsset(
                    peer  = liability,
                    scale = scale,
                    owner = this
                ).insert()
            }
        }
        
        def loseDerivative(name: String, scale: Scale):
            Seq[(DerivativeLiability, Scale)] =
        trans {
            val origLiab = DerivativeLiability.byName(name) getOrElse {
                // TODO: We need to do this just a little bit better......
                throw new IllegalStateException("The derivative vanished...")
            }
            val myAsset = DerivativeAsset.byPeer(origLiab, this)
            
            myAsset match {
                case None =>
                    val newLiab = replicateLiability(origLiab, scale)
                    (newLiab, Scale("1.0")) :: Nil
                    
                case Some(myAsset) =>
                    if (myAsset.scale >= scale) {
                        myAsset.scale -= scale
                        myAsset.update()
                        (origLiab, scale) :: Nil
                    }
                    else {
                        val newLiab = replicateLiability(origLiab, scale - myAsset.scale)
                        myAsset.delete()
                        (origLiab, myAsset.scale) :: (newLiab, Scale("1.0")) :: Nil
                    }
            }
        }
        
        def replicateLiability(liab: DerivativeLiability, scale: Scale):
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
        
        def castBid(auction: AuctionOffer, amount: Dollars): Unit = trans {
            val going = auction.goingPrice
            if (amount <= going) throw BidTooSmall(going)
            
            val bid = AuctionBid(
                offer = auction,
                by    = this,
                price = amount
            )
            bid.insert()
        }
        
        def myAuctionOffers: Seq[AuctionOffer] = trans {
            from(auctionOffers)( a =>
                where(a.offerer === this)
                select(a)
            ) toList
        }
    }

    case class StockAsset(
        var id:            Long             = 0,
        var ticker:        String           = "",
        var shares:        Shares           = Shares(0),
        var portfolio:     Link[Portfolio]  = 0
        )
        extends KL
    
    case class DerivativeAsset(
        var id:    Long            = 0,
        var peer:  Link[DerivativeLiability] = 0,
        var scale: Scale           = Scale(0),
        var owner: Link[Portfolio] = 0
        )
        extends KL
        with Loggable
    {
        def derivative: Derivative = trans {
            peer.derivative * scale
        }
        
        def executeManually(): Unit = trans {
            val deriv = derivative
            if (! deriv.early) throw NotExecutable
            
            executeUnchecked()
        }
        
        def executeOnSchedule(): Unit = trans {
            // Assuming the condition has already been checked!
            executeUnchecked()
        }
        
        protected def executeUnchecked(): Unit = trans {
            val deriv = derivative
            val secs  = deriv.securities
            
            for (sec <- secs) {
                owner.take(sec, peer.owner)
            }
            
            peer.reduceScale(scale)
            this.delete()
        }
    }
    object DerivativeAsset {
        def byPeer(liab: DerivativeLiability, owner: Portfolio): Option[DerivativeAsset] =
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
        var remaining:  Scale           = Scale("1.0"),
        var exec:       Timestamp       = now,
        var owner:      Link[Portfolio] = 0
        )
        extends KL
    {
        def derivative: Derivative = trans {
            Derivative.deserialize(mode)
        }
        
        def reduceScale(scale: Scale): Unit = trans {
            remaining -= scale
            
            if (remaining <= Scale(0)) this.delete()
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
        var to:     Link[Portfolio] = 0,
        var price:  Dollars         = Dollars("0.0")
        )
        extends KL
    {
        def derivative: Derivative = Derivative.deserialize(mode)
    }
    
    case class AuctionOffer(
        var id:       Long             = 0,
        var mode:     Array[Byte]      = Array(),
        var offerer:  Link[Portfolio]  = 0,
        var price:    Dollars          = Dollars("0.0"),
        var when:     Timestamp        = now,
        var expires:  Timestamp        = now
        )
        extends KL
    {
        def derivative: Derivative = Derivative.deserialize(mode)
        
        def goingPrice: Dollars = trans {
            val head = from(auctionBids)(bid =>
                where(bid.offer === this)
                compute(max(bid.price))
            ) headOption
            
            head getOrElse price
        }
        
        def highBid: Option[AuctionBid] = trans {
            val bids = from(auctionBids)(bid =>
                where(bid.offer === this)
                select(bid)
            ) toList
            
            if (bids.isEmpty) None
            else Some(bids maxBy (_.price))
        }
        
        def close(): Unit = trans {
            highBid match {
                case None =>
                    this.delete()
                    
                case Some(bid) =>
                    actOn(bid)
            }
        }
        
        def actOn(bid: AuctionBid): Unit = trans {
            offerer.takeCash(bid.price, bid.by)
            
            val deriv = derivative
            
            val liab =
                DerivativeLiability(
                    mode  = mode,
                    exec  = new Timestamp(deriv.exec.getMillis),
                    owner = offerer
                )
            liab.insert()
            
            val asset =
                DerivativeAsset(
                    peer  = liab,
                    scale = Scale("1.0"),
                    owner = bid.by
                )
            asset.insert()
            this.delete()
        }
    }
    object AuctionOffer {
        def byID(id: Long): AuctionOffer = trans {
            val opt = from(auctionOffers)(o =>
                where(o.id === id)
                select(o)
            ) headOption
            
            opt getOrElse (throw NoSuchAuction)
        }
    }
    
    case class AuctionBid(
        var id:    Long                = 0,
        var offer: Link[AuctionOffer]  = 0,
        var by:    Link[Portfolio]     = 0,
        var price: Dollars             = Dollars("0")
        )
        extends KL
    {
    }
        
    case class NewsEvent(
        var id:        Long       = 0,
        var when:      Timestamp  = now,
        var action:    String     = "",
        var subject:   Link[User] = 0,
        var recipient: Link[User] = 0,
        var ticker:    String     = "",
        var shares:    Shares     = Shares(0),
        var price:     Price      = Price(0)
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
    
    def schemaDDL: String = {
        var schema: String = ""
        trans {
            this.printDdl {st =>
                schema += st
                schema += "\n"
                schema += "\n"
            }
        }
        
        schema
    }
}

