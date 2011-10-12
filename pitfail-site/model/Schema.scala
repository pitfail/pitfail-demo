
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
import links.Sugar._
import java.sql.Timestamp

import Stocks.{StockShares, stockPrice}
import derivatives.{Derivative}

object Schema extends squeryl.Schema {
    
    implicit val users       = table[User]
    implicit val portfolios  = table[Portfolio]
    implicit val stockAssets = table[StockAsset]
    implicit val newsEvents  = table[NewsEvent]
    implicit val derivativeAssets      = table[DerivativeAsset]
    implicit val derivativeLiabilities = table[DerivativeLiability]
    implicit val derivativeOffers      = table[DerivativeOffer]
    
    def byUsername(name: String): Option[User] =
        inTransaction {
            from(users) (u =>
                where(u.username === name)
                select(u)
            ) headOption
        }
    
    def touchUser(username: String): User =
        inTransaction {
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
                    users update user
                    
                    user
                    
                case Some(user) => user
            }
        }
    
    def eventsSinceDays(n: Int): Seq[NewsEvent] = {
        val msecs = System.currentTimeMillis - (n: Long) * 24*60*60
        val stamp = new Timestamp(msecs)
        
        inTransaction {
            from(newsEvents)(e =>
                where(e.when > stamp)
                select(e)
            )
            .toList
        }
    }
    
    def recentEvents(n: Int): Seq[NewsEvent] = inTransaction {
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
    }
    
    case class Portfolio(
        var id:            Long             = 0,
        var cash:          BigDecimal       = 0,
        var owner:         Link[User]       = 0
        )
        extends KL
    {
        def buy(stock: StockShares): StockAsset = inTransaction {
            assert(cash >= stock.price)
            
            // Look to see if we can lump this in with an
            // existing asset
            val asset =
                haveTicker(stock.ticker) match {
                    case Some(asset) => asset
                    case None        => makeTicker(stock.ticker)
                }
            
            asset.shares += stock.shares
            cash -= stock.value
            assert(cash > 0)

            newsEvents insert NewsEvent(
                action  = "buy",
                subject = owner,
                ticker  = stock.ticker,
                shares  = stock.shares,
                price   = stock.price
            )
            portfolios.update(this)
            stockAssets.update(asset)
            
            asset
        }
        
        def sell(stock: StockShares): Unit = inTransaction {
            val asset =
                haveTicker(stock.ticker) match {
                    case Some(asset) => asset
                    case None =>
                        throw new IllegalStateException(
                            "Cannot sell %s; don't have it" format stock.ticker
                        )
                }
            
            assert(asset.shares >= stock.shares)
            
            cash += stock.value
            asset.shares -= stock.shares
            if (asset.shares <= 0)
                stockAssets.deleteWhere(a => a.id === asset.id)
            else
                stockAssets.update(asset)
            
            newsEvents insert NewsEvent(
                action  = "sell",
                subject = owner,
                ticker  = stock.ticker,
                shares  = stock.shares,
                price   = stock.price
            )
            portfolios.update(this)
        }
        
        def sellAll(ticker: String): Unit = inTransaction {
            val asset =
                haveTicker(ticker) match {
                    case Some(asset) => asset
                    case None =>
                        throw new IllegalStateException(
                            "Cannot sell %s; don't have it" format ticker
                        )
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
            stockAssets.deleteWhere(a => a.id === asset.id)
            portfolios.update(this)
        }
        
        def haveTicker(ticker: String): Option[StockAsset] =
            inTransaction {
                from(stockAssets) (a =>
                    where(
                            (a.ticker === ticker)
                        and (a.portfolio === this)
                    )
                    select(a)
                ) headOption
            }
        
        def makeTicker(ticker: String): StockAsset =
            inTransaction {
                val asset = StockAsset(
                    ticker    = ticker,
                    portfolio = this
                )
                stockAssets.insert(asset)
                asset
            }
        
        def myStockAssets: Seq[StockAsset] =
            inTransaction {
                from(stockAssets) (a =>
                    where(
                        a.portfolio === this
                    )
                    select(a)
                ) toList
            }
        
        def myDerivativeAssets: Seq[DerivativeAsset] =
            inTransaction {
                from(derivativeAssets) (a =>
                    where (
                        a.owner === this
                    )
                    select(a)
                ) toList
            }
        
        def myOffers: Seq[DerivativeOffer] =
            inTransaction {
                from (derivativeOffers) (o =>
                    where (
                        o.to === this
                    )
                    select(o)
                ) toList
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
        var name:  String          = "",
        var mode:  String          = "",
        var exec:  Timestamp       = now,
        var peer:  Link[Portfolio] = 0,
        var owner: Link[Portfolio] = 0
        )
        extends KL
    {
        def derivative: Derivative = Derivative.fromJSON(mode)
    }
    
    case class DerivativeLiability(
        var id:    Long            = 0,
        var mode:  String          = "",
        var owner: Link[Portfolio] = 0
        )
        extends KL
    {
        def derivative: Derivative = Derivative.fromJSON(mode)
    }
    
    case class DerivativeOffer(
        var id:   Long            = 0,
        var mode: String          = "",
        @Column("sender")
        var from: Link[Portfolio] = 0,
        var to:   Link[Portfolio] = 0
        )
        extends KL
    {
        def derivative: Derivative = Derivative.fromJSON(mode)
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
        
        inTransaction {
            this.create
        }
    }
}

