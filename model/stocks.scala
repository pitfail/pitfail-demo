
package model

import scala.math.BigDecimal
import stockdata._
import org.joda.time.DateTime
import org.joda.time.Duration

import scala.collection.JavaConversions._

trait StockSchema {
    schema: UserSchema with DBMagic with SchemaErrors with NewsSchema =>
    
    implicit val stockAssets: Table[StockAsset] = table[StockAsset]
    
    // Model Tables
    
    case class StockAsset(
            id:     Key = nextID,
            ticker: String,
            shares: Shares,
            owner:  Link[Portfolio],
            purchasePrice: Price,
            notifiedPrice: Price
        )
        extends KL
        with StockAssetOps

    case class StockPurchase(
            shares:  Shares,
            dollars: Dollars,
            asset: Key
        )
    
    // Operations
    
    trait StockAssetOps {
        self: StockAsset =>
            
        def price: Price = Stocks.stockPrice(ticker)
        def dollars: Dollars = shares * price
    }
    
    trait PortfolioWithStocks {
        self: Portfolio =>
        
        def myStockAssets = stockAssets filter (_.owner ~~ self) toList
        
        // Java interop
        def getMyStockAssets: java.util.List[StockAsset] = readDB(myStockAssets)
        
        def howManyShares(ticker: String) = readDB {
            haveTicker(ticker) map (_.shares) getOrElse Shares(0)
        }
        
        def howManyDollars(ticker: String) =
            howManyShares(ticker) * Stocks.stockPrice(ticker)
        
        def userBuyStock(ticker: String, shares: Shares) =
            editDB(buyStock(ticker, shares))
        
        def userBuyStock(ticker: String, dollars: Dollars) =
            editDB(buyStock(ticker, dollars))
        
        def userSellStock(ticker: String, shares: Shares) =
            editDB(sellStock(ticker, shares))
        
        def userSellStock(ticker: String, dollars: Dollars) =
            editDB(sellStock(ticker, dollars))
        
        def userSellAll(ticker: String) = editDB(sellAll(ticker))
        
        // Buy a stock in shares
        private[model] def buyStock(ticker: String, shares: Shares): Transaction[StockPurchase] = {
            val price = Stocks.stockPrice(ticker)
            buyStock(ticker, price * shares, shares, price)
        }
        
        // Buy a stock in dollars
        private[model] def buyStock(ticker: String, dollars: Dollars): Transaction[StockPurchase] = {
            val price = Stocks.stockPrice(ticker)
            try {
                buyStock(ticker, dollars, dollars /-/ price, price)
            }
            catch {
                case e: ArithmeticException =>
                    logger.info("Failed to buy because the price is zero")
                    logger.info("%s %s" format (ticker.toString, price.toString))
                    // TODO: This is not good
                    buyStock(ticker, Dollars(1), Shares(1), Price(1))
            }
        }
        
        private[model] def buyStock(
                ticker: String, dollars: Dollars, shares: Shares, price: Price) =
        {
            if (cash <= dollars) throw NotEnoughCash(have=cash, need=dollars)
            
            for {
                asset <- ensureAsset(ticker)
                _ <- asset update (a => a copy (
                    shares = a.shares + shares,
                    purchasePrice = (a.purchasePrice*a.shares + price*shares) / (a.shares+shares)
                ))
                _ <- this update (t => t copy (cash=t.cash-dollars))
                
                // Report the event
                _  <- Bought(this, ticker, shares, dollars, price).report
            }
            yield StockPurchase(shares, dollars, asset.id)
        }
        
        // Create this stock asset if it does not already exist
        private[model] def ensureAsset(ticker: String) = haveTicker(ticker) match {
            case Some(asset) => Transaction(asset)
            case None        =>
                StockAsset(ticker=ticker,shares=Shares(0),owner=this,
                    purchasePrice=Price(0),notifiedPrice=Price(0)).insert
        }
        
        // Sell a stock in shares
        private[model] def sellStock(ticker: String, shares: Shares): Transaction[Unit] = {
            val price = Stocks.stockPrice(ticker)
            sellStock(ticker, shares*price, shares, price)
        }
        
        // Sell a stock in dollars
        private[model] def sellStock(ticker: String, dollars: Dollars): Transaction[Unit] = {
            val price = Stocks.stockPrice(ticker)
            sellStock(ticker, dollars, dollars /-/ price, price)
        }
        
        private[model] def sellStock
                (ticker: String, dollars: Dollars, shares: Shares, price: Price) =
        {
            val asset = haveTicker(ticker) getOrElse (throw DontOwnStock(ticker))
            if (asset.shares < shares) throw NotEnoughShares(have=asset.shares, need=shares)
            
            val assetChange =
                if (asset.shares > shares) asset update (a => a copy (shares=a.shares-shares))
                else asset.delete
            
            for {
                _ <- assetChange
                _ <- this update (t => t copy (cash=t.cash+dollars))
                
                // Report it to the news!
                _ <- Sold(this, ticker, shares, dollars, price).report
            }
            yield ()
        }
        
        // Sell all of a single stock
        private[model] def sellAll(ticker: String): Transaction[Unit] = {
            val asset = haveTicker(ticker) getOrElse (throw DontOwnStock(ticker))
            val price = Stocks.stockPrice(ticker)
            val dollars = asset.shares * price
            
            for {
                _ <- asset.delete
                _ <- this update (t => t copy (cash=t.cash+dollars))
                _ <- Sold(this, ticker, asset.shares, dollars, price).report
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
