
package model

import scala.math.BigDecimal
import stockdata._
import org.joda.time.DateTime
import org.joda.time.Duration

trait StockSchema {
    schema: UserSchema with DBMagic with SchemaErrors with NewsSchema =>
    
    implicit val stockAssets: Table[StockAsset] = table[StockAsset]
    
    // Model Tables
    
    case class StockAsset(
            id:     Key = nextID,
            ticker: String,
            shares: Shares,
            owner:  Link[Portfolio]
        )
        extends KL

    case class StockPurchase(
            asset:   StockAsset,
            shares:  Shares,
            dollars: Dollars
        )

    // Operations
    
    trait PortfolioWithStocks {
        self: Portfolio =>
        
        def myStockAssets = stockAssets filter (_.owner ~~ self) toList
        
        // Buy a stock in shares
        def buyStock(ticker: String, shares: Shares): Transaction[StockPurchase] = {
            val price = Stocks.stockPrice(ticker)
            buyStock(ticker, price * shares, shares, price)
        }
        
        // Buy a stock in dollars
        def buyStock(ticker: String, dollars: Dollars): Transaction[StockPurchase] = {
            val price = Stocks.stockPrice(ticker)
            buyStock(ticker, dollars, dollars /-/ price, price)
        }
        
        protected def buyStock(
                ticker: String, dollars: Dollars, shares: Shares, price: Price) =
        {
            if (cash <= dollars) throw NotEnoughCash(have=cash, need=dollars)
            
            for {
                asset <- ensureAsset(ticker)
                asset <- asset.copy(shares = asset.shares+shares).update
                _     <- this.copy(cash=cash-dollars).update
                
                // Report the event
                _  <- addBuyEvent(owner, ticker, shares, price)
            }
            yield StockPurchase(asset, shares, dollars)
        }
        
        // Create this stock asset if it does not already exist
        protected def ensureAsset(ticker: String) = haveTicker(ticker) match {
            case Some(asset) => Transaction(asset)
            case None        => StockAsset(ticker=ticker,shares=Shares(0),owner=this).insert
        }
        
        // Sell a stock in shares
        def sellStock(ticker: String, shares: Shares): Transaction[Unit] =
            sellStock(ticker, shares*Stocks.stockPrice(ticker), shares)
        
        // Sell a stock in dollars
        def sellStock(ticker: String, dollars: Dollars): Transaction[Unit] =
            sellStock(ticker, dollars, dollars /-/ Stocks.stockPrice(ticker))
        
        protected def sellStock(ticker: String, dollars: Dollars, shares: Shares) = {
            val asset = haveTicker(ticker) getOrElse (throw DontOwnStock(ticker))
            if (asset.shares < shares) throw NotEnoughShares(have=asset.shares, need=shares)
            
            val newAsset = asset.copy(shares = asset.shares-shares)
            val assetChange = if (newAsset.shares > Shares(0)) newAsset.update else asset.delete
            
            for {
                _ <- assetChange
                _ <- this.copy(cash = cash+dollars).update
            }
            yield ()
        }
        
        // Sell all of a single stock
        def sellAll(ticker: String): Transaction[Unit] = {
            val asset = haveTicker(ticker) getOrElse (throw DontOwnStock(ticker))
            val dollars = asset.shares * Stocks.stockPrice(ticker)
            
            for {
                _ <- asset.delete
                _ <- this.copy(cash = cash+dollars).update
            }
            yield ()
        }
        
        // Get a stock asset if we have it
        def haveTicker(ticker: String): Option[StockAsset] =
            schema.stockAssets filter (a => a.owner~~self && a.ticker~~ticker) headOption
    }
}

// -------------------------------------------------------------------------
// Price fetching

object StockPriceSource extends CachedStockDatabase(
    new FailoverStockDatabase(List(
      new YahooCSVStockDatabase(new HttpQueryService("GET")),
      new YahooStockDatabase(new HttpQueryService("GET"))
    )),
    // TODO: This timeout should be moved to a configuration file.
    new Duration(1000 * 60 * 5)
)

object Stocks {
    def stockPrice(ticker: String): Price = {
        val stock = Stock(ticker)
        val quote = StockPriceSource.getQuotes(Seq(stock)).head
        quote.price
    }
}

