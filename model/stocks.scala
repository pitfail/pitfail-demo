
package model

import scala.math.BigDecimal
import stockdata._
import org.joda.time.DateTime
import org.joda.time.Duration

import scala.collection.JavaConversions._
import scalaz.Scalaz._
import scala.collection.mutable.{Map => MMap}
import spser._

trait StockSchema extends Schema {
    schema: UserSchema with DBMagic with SchemaErrors with NewsSchema =>
    
    implicit val saCon = StockAsset.apply _
    implicit val sbhCon = StockBuyHistory.apply _
    implicit val sshCon = StockSellHistory.apply _
    implicit val bloCon = BuyLimitOrder.apply _
    implicit val sloCon = SellLimitOrder.apply _
        
    implicit val stockAssets: Table[StockAsset] = table[StockAsset]
    implicit val stockBuyHistories: Table[StockBuyHistory] = table[StockBuyHistory]
    implicit val stockSellHistories: Table[StockSellHistory] = table[StockSellHistory]
    implicit val buyLimitOrders: Table[BuyLimitOrder] = table[BuyLimitOrder]
    implicit val sellLimitOrders: Table[SellLimitOrder] = table[SellLimitOrder]
    
    abstract override def tables = (stockAssets :: stockBuyHistories ::
        stockSellHistories :: buyLimitOrders :: sellLimitOrders :: super.tables)
    
    // Model Tables
    
    case class StockAsset(
            id:     Key = nextID,
            ticker: String,
            shares: Shares,
            owner:  Link[Portfolio],
            notifiedPrice: Price,
            lastDividendDate: DateTime,
            totalDividends:   Dollars
        )
        extends KL
        with StockAssetOps

    case class StockBuyHistory(
            id:       Key = nextID,
            owner:    Link[Portfolio],
            ticker:   String,
            buyDate:  DateTime,
            buyPrice: Price,
            shares:   Shares,
            dollars:  Dollars
        )
        extends KL
        
    case class StockSellHistory(
            id:        Key = nextID,
            owner:     Link[Portfolio],
            ticker:    String,
            sellDate:  DateTime,
            sellPrice: Price,
            shares:    Shares,
            dollars:   Dollars
        )
        extends KL
        
    case class BuyLimitOrder(
            id:       Key = nextID,
            ticker:   String,
            shares:   Shares,
            owner:    Link[Portfolio],
            limit:    Price,
            setAside: Dollars
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
    
    trait StockAssetOps {
        self: StockAsset =>
            
        def price: Price = Stocks.lastTradePrice(ticker)
        def dollars: Dollars = shares * price
        
        def buyHistories = (stockBuyHistories where
            ('ticker ~=~ ticker) where ('owner ~=~ owner)) toList
            
        def sellHistories = (stockSellHistories where
            ('ticker ~=~ ticker) where ('owner ~=~ owner) toList)
        
        def averagePurchasePrice = readDB {
            val hists = buyHistories
            
            val dollars = (hists map (_.dollars)).foldLeft(Dollars(0))(_+_)
            val shares = (hists map (_.shares)).foldLeft(Shares(0))(_+_)
            
            dollars / shares
        }
    }
    
    trait PortfolioWithStocks {
        self: Portfolio =>
        
        def myStockAssets = stockAssets where ('owner ~=~ self) toList
        
        def haveTicker(ticker: String): Option[StockAsset] =
            stockAssets where ('owner ~=~ this) where ('ticker ~=~ ticker) headOption
        
        // Java interop
        def getMyStockAssets: java.util.List[StockAsset] = readDB(myStockAssets)
        
        def howManyShares(ticker: String) = readDB {
            haveTicker(ticker) match {
                case Some(a) => a.shares
                case None    => Shares(0)
            }
        }
        
        def howManyDollars(ticker: String) =
            howManyShares(ticker) * Stocks.lastTradePrice(ticker)
        
        def userBuyStock(ticker: String, shares: Shares) =
            editDB(buyStock(ticker, shares))
        
        def userBuyStock(ticker: String, dollars: Dollars) =
            editDB(buyStock(ticker, dollars))
        
        def userSellStock(ticker: String, shares: Shares) =
            editDB(sellStock(ticker, shares))
        
        def userSellStock(ticker: String, dollars: Dollars) =
            editDB(sellStock(ticker, dollars))
        
        def userSellAll(ticker: String) = editDB(sellAll(ticker))
        
        def userMakeBuyLimitOrder(ticker: String, shares: Shares, limit: Price) = editDB {
            makeBuyLimitOrder(ticker, shares, limit)
        }
        
        def userMakeSellLimitOrder(ticker: String, shares: Shares, limit: Price) = editDB {
            makeSellLimitOrder(ticker, shares, limit)
        }
        
        def myBuyLimitOrders = buyLimitOrders where ('owner ~=~ this) toList
        
        def mySellLimitOrders = sellLimitOrders where ('owner ~=~ this) toList
        
        // There could be multiple sources of margin. Right now we have only this one
        def margin: Dollars = myBuyLimitOrders map (_.setAside) summ
        
        // Buy a stock in dollars
        private[model] def buyStock(ticker: String, dollars: Dollars): Transaction[StockPurchase] = {
            val price = Stocks.lastTradePrice(ticker)
            buyStock(ticker, dollars /-/ price)
        }
        
        // Buy a stock in shares
        private[model] def buyStock(ticker: String, shares: Shares): Transaction[StockPurchase] = {
            def buyEach(trades: List[Trade]) = {
                val each = trades map { trade =>
                    for {
                        _ <- Bought(this, ticker, trade.shares, trade.dollars, trade.price).report
                        _ <- StockBuyHistory(owner=this, ticker=ticker, buyDate=new DateTime,
                            buyPrice=trade.price, shares=trade.shares, dollars=trade.dollars).insert
                    }
                    yield ()
                }
            
                each.sequence
            }
            
            def buyAll = haveTicker(ticker) match {
                case None => StockAsset(ticker=ticker, shares=shares, owner=this,
                    notifiedPrice=Price(0), lastDividendDate=new DateTime, totalDividends=Dollars(0)).insert
                
                case Some(asset) =>
                    asset update (a => a copy (shares=a.shares+shares))
            }
            
            // These are already sorted in the most desirable order
            val sellers = sellersFor(ticker)
            for {
                trades <- processTradeables(sellers, shares)
                spent = Dollars((trades map (_.dollars.dollars)).sum)
                _      <- buyEach(trades)
                _      <- buyAll
                _      <- this update (t => t copy(cash=t.cash-spent))
            }
            yield {
                if (spent > cash) throw NotEnoughCash(have=cash, need=spent)
                StockPurchase(shares, spent)
            }
        }
        
        // Sell a stock in dollars
        private[model] def sellStock(ticker: String, dollars: Dollars): Transaction[Unit] = {
            val price = Stocks.lastTradePrice(ticker)
            sellStock(ticker, dollars /-/ price)
        }
        
        // Sell a stock in shares
        private[model] def sellStock(ticker: String, askShares: Shares): Transaction[Unit] = {
            val asset = 
                haveTicker(ticker) match {
                    case Some(asset) => asset
                    case None        => throw DontOwnStock(ticker)
                }
            
            val okShares =
                if (asset.shares*Scale("0.97") < askShares &&
                        askShares < asset.shares*Scale("1.03")) asset.shares
                else askShares
            
            if (okShares > asset.shares) throw NotEnoughShares(asset.shares, askShares)
            
            def sellEach(trades: List[Trade]) = {
                val each = trades map { trade =>
                    for {
                        _ <- Sold(this, ticker, trade.shares, trade.dollars, trade.price).report
                        _ <- StockSellHistory(owner=this, ticker=ticker, sellDate=new DateTime,
                            sellPrice=trade.price, shares=trade.shares, dollars=trade.dollars).insert
                    }
                    yield ()
                }
            
                each.sequence
            }
            
            def sellAll =
                if (okShares >= asset.shares) asset.delete
                else asset update (a => a copy(shares = a.shares-okShares))
            
            val buyers = buyersFor(ticker)
            for {
                trades <- processTradeables(buyers, okShares)
                dollars = Dollars((trades map (_.dollars.dollars)).sum)
                _      <- sellEach(trades)
                _      <- sellAll
                _      <- this update (t => t copy (cash=t.cash+dollars))
            }
            yield ()
        }
        
        def processTradeables
            (tradeables: List[Tradeable], shares: Shares)
            : Transaction[List[Trade]] =
        {
            processTradeables(tradeables, shares, Nil)
        }
        
        // This is a recursive monadic function I'M SORRY
        def processTradeables
            (tradeables: List[Tradeable], sharesRemaining: Shares, trades: List[Trade])
            : Transaction[List[Trade]] =
        {
            if (sharesRemaining <= Shares(0)) {
                Transaction(trades)
            }
            else tradeables match {
                // Maybe we should queue this as some kind of order?
                // There are reasons I don't want to do that.
                case Nil => throw NoBidders
                case tradeable :: rest =>
                    if (tradeable.available > sharesRemaining)
                        for {
                            dollars <- tradeable.satisfyPartially(sharesRemaining)
                        }
                        yield Trade(sharesRemaining, dollars) :: trades
                    else
                        for {
                            dollars <- tradeable.satisfyCompletely
                            s <- processTradeables(rest, sharesRemaining-tradeable.available,
                                Trade(tradeable.available, dollars) :: trades)
                        }
                        yield s
            }
        }
        
        // Sell all of a single stock
        private[model] def sellAll(ticker: String): Transaction[Unit] = {
            val asset = 
                haveTicker(ticker) match {
                    case Some(asset) => asset
                    case None        => throw DontOwnStock(ticker)
                }
            
            sellStock(ticker, asset.shares)
        }
        
        private[model] def makeBuyLimitOrder(ticker: String, shares: Shares, limit: Price) = {
            // First see if anyone already wants to sell
            val sellers = sellersFor(ticker) filter (_.price <= limit)
            val assets = haveTicker(ticker).toList
            
            val availableShares = (sellers map (_.available)).summ
            if (availableShares >= shares) {
                buyStock(ticker, shares)
            }
            else {
                val buy =
                    if (availableShares > Shares(0)) buyStock(ticker, availableShares)
                    else Transaction(())
                    
                val remaining = shares - availableShares
                val setAside  = remaining * limit
                
                for {
                    _ <- buy
                    _ <- BuyLimitOrder(ticker=ticker, shares=remaining, owner=this,
                        limit=limit, setAside=setAside).insert
                    _ <- BuyOrdered(buyer=this, stock=ticker, shares=remaining, limit=limit).report
                    _ <- this update (t => t copy (cash=t.cash-setAside))
                }
                yield {
                    if (setAside > cash) throw NotEnoughCash(cash, setAside)
                }
            }
        }
        
        private[model] def makeSellLimitOrder(ticker: String, shares: Shares, limit: Price) = {
            val asset = haveTicker(ticker) match {
                case None => throw DontOwnStock(ticker)
                case Some(asset) => asset
            }
            
            if (asset.shares < shares) throw NotEnoughShares(asset.shares, shares)
            
            // First see if anyone already wants to buy
            val buyers = buyersFor(ticker) filter (_.price >= limit)
            
            val availableShares = (buyers map (_.available)).summ
            if (availableShares >= shares) {
                sellStock(ticker, shares)
            }
            else {
                val sell =
                    if (availableShares > Shares(0)) sellStock(ticker, availableShares)
                    else Transaction(())
                    
                val remaining = shares - availableShares
                    
                for {
                    _ <- sell
                    _ <- SellLimitOrder(ticker=ticker, shares=remaining,
                        owner=this, limit=limit).insert
                    _ <- SellOrdered(seller=this, stock=ticker, shares=remaining, limit=limit).report
                    _ <- asset update (a => a copy (shares=a.shares-remaining))
                }
                yield ()
            }
        }
    }
    
    trait Tradeable {
        val name: String
        val available: Shares
        val price: Price
        
        def satisfyPartially(shares: Shares): Transaction[Dollars]
        def satisfyCompletely: Transaction[Dollars]
    }
    
    class AutomaticTrader(ticker: String, premium: Scale, targetDollars: Dollars,
            name: String, buyer: Boolean)
    {
        self =>
        
        var reserve = targetDollars /-/ Stocks.lastTradePrice(ticker)
            
        def makeTradeable = new Tradeable {
            val name      = self.name
            val available = reserve
            val price     = 
                if (buyer) Stocks.bidPrice(ticker) * premium
                else Stocks.askPrice(ticker) * premium
                
            def satisfyPartially(shares: Shares) =
                Transaction(shares*price, List(new EditOp {
                    def perform() {
                        reserve -= shares
                    }
                    val affectedTables = Nil
                }))
            
            def satisfyCompletely = satisfyPartially(available)
        }
    }
    
    var automaticSellers = MMap[String,List[AutomaticTrader]]()
    var automaticBuyers  = MMap[String,List[AutomaticTrader]]()
    
    def initAutomaticBuyers(ticker: String) = List(
        new AutomaticTrader(ticker, Scale("1.00"), Dollars(20000), "Des. Market Maker #3", true),
        new AutomaticTrader(ticker, Scale(".97"), Dollars(13000), "Des. Market Maker #7", true),
        new AutomaticTrader(ticker, Scale(".93"), Dollars( 8000), "Des. Market Maker #12", true)
    )
    
    def initAutomaticSellers(ticker: String) = List(
        new AutomaticTrader(ticker, Scale("1.00"), Dollars(20000), "Des. Market Maker #3", true),
        new AutomaticTrader(ticker, Scale("1.04"), Dollars(13000), "Des. Market Maker #7", true),
        new AutomaticTrader(ticker, Scale("1.07"), Dollars( 8000), "Des. Market Maker #12", true)
    )
    
    def sellersFor(ticker: String): List[Tradeable] = {
        val auto = automaticSellers.getOrElseUpdate(ticker, {
            initAutomaticSellers(ticker)
        })
        
        // TODO: Other sellers
        auto map (_.makeTradeable) sortBy (_.price)
    }
    
    def buyersFor(ticker: String): List[Tradeable] = {
        val auto = automaticBuyers.getOrElseUpdate(ticker,
            initAutomaticBuyers(ticker))
        
        // TODO: Other buyers
        auto map (_.makeTradeable) sortBy (- _.price)
    }
    
    case class Trade(shares: Shares, dollars: Dollars) {
        val price = dollars / shares
    }
}

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

