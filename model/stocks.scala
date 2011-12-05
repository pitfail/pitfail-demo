
package model

import scala.math.BigDecimal
import stockdata._
import org.joda.time.DateTime
import org.joda.time.Duration

import scala.collection.JavaConversions._
import scalaz.Scalaz._

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
            notifiedPrice: Price,
            purchaseDate:     DateTime,
            lastDividendDate: DateTime,
            totalDividends:   Dollars
        )
        extends KL
        with StockAssetOps

    case class BuyLimitOrder(
            id:     Key = nextID,
            ticker: String,
            shares: Shares,
            owner:  Link[Portfolio],
            limit:  Price
        )
        extends KL
        
    case class SellLimitOrder(
            id:     Key = nextID,
            ticker: String,
            shares: Shares,
            owner:  Link[Portfolio],
            limit:  Price
        )
        extends KL
        
    // Operations
    
    case class StockPurchase(
            shares:  Shares,
            dollars: Dollars
        )
    
    // This is used to show all assets with the same ticker lumped together
    case class GroupedStockAsset(
            ticker: String,
            shares: Shares,
            owner: Link[Portfolio],
            purchasePrice: Price
        )
    
    trait StockAssetOps {
        self: StockAsset =>
            
        def price: Price = Stocks.stockPrice(ticker)
        def dollars: Dollars = shares * price
    }
    
    trait PortfolioWithStocks {
        self: Portfolio =>
        
        def myStockAssets = stockAssets filter (_.owner ~~ self) toList
        
        def myStockAssetsGrouped: Seq[GroupedStockAsset] =
            myStockAssets groupBy (_.ticker) map { case (ticker, assets) =>
                GroupedStockAsset(
                    ticker = ticker,
                    shares = Shares((assets map (_.shares.shares)).sum),
                    owner  = this,
                    purchasePrice = {
                        val dollars = assets map (a => a.shares*a.purchasePrice)
                        val shares = assets map (_.shares)
                        dollars.reduceLeft(_+_) / shares.reduceLeft(_+_)
                    }
                )
            } toList
        
        // Java interop
        def getMyStockAssets: java.util.List[StockAsset] = readDB(myStockAssets)
        
        def howManyShares(ticker: String) = readDB {
            val s = (myStockAssets filter (_.ticker==ticker) map (_.shares.shares)).sum
            Shares(s)
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
        
        // Buy a stock in dollars
        private[model] def buyStock(ticker: String, dollars: Dollars): Transaction[StockPurchase] = {
            val price = Stocks.lastTradePrice(ticker)
            buyStock(ticker, dollars, dollars /-/ price, price)
        }
        
        // Buy a stock in shares
        private[model] def buyStock(ticker: String, shares: Shares): Transaction[StockPurchase] = {
            def addStockAsset(shares: Shares, price: Price) =
                StockAsset(ticker=ticker, shares=shares, owner=this,
                    purchasePrice=price, notifiedPrice=price, purchaseDate=new DateTime,
                    lastDividendDate=new DateTime, totalDividends=Dollars(0)).insert
            
            // This is a recursive monadic function I'M SORRY
            def processSellables
                (sellables: List[Sellable], sharesRemaining: Shares, dollarsSpent: Dollars)
                : Transaction[Dollars] =
            {
                if (remaining <= Shares(0)) {
                    Transaction.pure(dollarsSpent)
                }
                else sellables match {
                    // Maybe we should queue this as some kind of order?
                    // There are reasons I don't want to do that.
                    case Nil => throw NoBidders
                    case sellable :: rest =>
                        if (sellable.available > sharesRemaining)
                            for {
                                dollars <- sellable.satisfyPartially(sharesRemaining)
                                _ <- addStockAsset(sharesRemaining, sellable.askPrice)
                            }
                            yield {
                                dollarsSpent + dollars
                            }
                        else
                            for {
                                dollars <- sellable.satisfyCompletely
                                _ <- addStockAsset(sellable.available, sellable.askPrice)
                                s <- processSellables(rest, sharesRemaining-sellable.available, dollarsSpent+dollars)
                            }
                            yield {
                                s
                            }
                }
            }
            
            val sellables = sellablesFor(ticker) orderBy (_.askPrice)
            for {
                spent <- processSellables(sellables, shares, Dollars(0))
                _ <- this update (t => t copy(cash=t.cash-spent))
                _ <- Bought(this, ticker, shares, spent, spent/shares).report
            }
            yield {
                if (spent > cash) throw NotEnoughCash(have=cash, need=spent)
                StockPurchase(shares, spent)
            }
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
            val allAssets = myStockAssets filter (_.ticker==ticker) sortBy (_.shares.shares)
            
            def processAssets(soFar: Shares, assets: List[StockAsset]): Transaction[Unit] = {
                assets match {
                    case Nil =>
                        if (allAssets.isEmpty) throw DontOwnStock(ticker)
                        else throw NotEnoughShares(soFar, shares)
                        
                    case asset::restAssets =>
                        if (asset.shares+soFar == shares)
                            asset.delete map (_ => ())
                        
                        else if (asset.shares+soFar >= shares) {
                            val remaining = shares - soFar
                            asset update (a => a copy (shares=a.shares-remaining))
                        }
                        else
                            asset.delete flatMap (_ =>
                                processAssets(soFar-asset.shares, restAssets)
                            )
                }
            }
            
            for {
                _ <- processAssets(Shares(0), allAssets toList)
                _ <- this update (t => t copy (cash=t.cash+dollars))
                
                // Report it to the news!
                _ <- Sold(this, ticker, shares, dollars, price).report
            }
            yield ()
        }
        
        // Sell all of a single stock
        private[model] def sellAll(ticker: String): Transaction[Unit] = {
            val assets = myStockAssets filter (_.ticker==ticker)
            
            val delete = (assets map (_.delete)).sequence
            val totalShares = Shares((assets map (_.shares.shares)).sum)
            val price = Stocks.stockPrice(ticker)
            val dollars = totalShares * price
            
            for {
                _ <- delete
                _ <- this update (t => t copy (cash=t.cash+dollars))
                _ <- Sold(this, ticker, totalShares, dollars, price).report
            }
            yield ()
        }
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

object DividendSource extends CachedDividendDatabase(
    new YahooDividendDatabase(new HttpQueryService("GET"))
)

object Stocks {
    var syntheticDividends: List[Dividend] = List()
    
    def askPrice(ticker: String): Price = {
        val stock = Stock(ticker)
        val quote = StockPriceSource.getQuotes(Seq(stock)).head
        quote.askPrice
    }
    
    def bidPrice(ticker: String): Price = {
        val stock = Stock(ticker)
        val quote = StockPriceSource.getQuotes(Seq(stock)).head
        quote.bidPrice
    }
    
    def lastTradePrice(ticker: String): Price = {
        val stock = Stock(ticker)
        val quote = StockPriceSource.getQuotes(Seq(stock)).head
        quote.price
    }
    
    def recentDividends(ticker: String): Seq[Dividend] = {
        val actual = DividendSource recentDividends Stock(ticker)
        actual ++ (syntheticDividends filter (_.ticker==ticker))
    }
}

