
package model

import scala.math.BigDecimal
import stockdata._
import org.joda.time.DateTime
import org.joda.time.Duration

import scala.collection.JavaConversions._
import scalaz.Scalaz._
import spser._

trait StockSchema extends Schema {
    schema: UserSchema with DBMagic with SchemaErrors with NewsSchema =>
    
    implicit val saCon = StockAsset.apply _
        
    implicit val stockAssets: Table[StockAsset] = table[StockAsset]
    
    abstract override def tables = stockAssets :: super.tables
    
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

    case class StockPurchase(
            shares:  Shares,
            dollars: Dollars,
            asset: Key
        )
    
    // Operations
    
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
        
        def myStockAssets = stockAssets where ('owner ~=~ self) toList
        
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
        
        def myAssetsForTicker(ticker: String) = (stockAssets
            where ('owner ~=~ self) where ('ticker ~=~ ticker)).toList
        
        def howManyShares(ticker: String) = readDB {
            val s = (myAssetsForTicker(ticker) map (_.shares.shares)).sum
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
        
        // Buy a stock in shares
        private[model] def buyStock(ticker: String, shares: Shares): Transaction[StockPurchase] = {
            val price = Stocks.stockPrice(ticker)
            buyStock(ticker, price * shares, shares, price)
        }
        
        // Buy a stock in dollars
        private[model] def buyStock(ticker: String, dollars: Dollars): Transaction[StockPurchase] = {
            val price = Stocks.stockPrice(ticker)
            buyStock(ticker, dollars, dollars /-/ price, price)
        }
        
        private[model] def buyStock(
                ticker: String, dollars: Dollars, shares: Shares, price: Price) =
        {
            if (cash <= dollars) throw NotEnoughCash(have=cash, need=dollars)
            
            for {
                asset <- StockAsset(ticker=ticker, shares=shares, owner=this,
                        purchasePrice=price, notifiedPrice=price, purchaseDate=new DateTime,
                        lastDividendDate=new DateTime, totalDividends=Dollars(0)
                    ).insert
                
                _ <- this update (t => t copy (cash=t.cash-dollars))
                
                // Report the event
                _  <- Bought(this, ticker, shares, dollars, price).report
            }
            yield StockPurchase(shares, dollars, asset.id)
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
            val allAssets = myAssetsForTicker(ticker) sortBy (_.shares.shares)
            
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
            val assets = myAssetsForTicker(ticker)
            
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

object Stocks {
    var syntheticDividends: List[Dividend] = List()
    
    def stockPrice(ticker: String): Price = {
        val stock = Stock(ticker)
        val quote = StockPriceSource.getQuotes(Seq(stock)).head
        quote.price
    }
    
    def recentDividends(ticker: String): Seq[Dividend] = {
        val actual = DividendSource recentDividends Stock(ticker)
        actual ++ (syntheticDividends filter (_.ticker==ticker))
    }
}

