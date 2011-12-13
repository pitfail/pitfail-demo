
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
        
    // ref_225
    case class BuyLimitOrder(
            id:       Key = nextID,
            ticker:   String,
            shares:   Shares,
            owner:    Link[Portfolio],
            limit:    Price,
            // This is margin set aside for buying
            setAside: Dollars
        )
        extends KL
        with BuyLimitOrderOps
        with OrderAttrs
        
    case class SellLimitOrder(
            id:     Key = nextID,
            ticker: String,
            // These shares have been set aside. Credit them
            // back to the portfolio if the order is cancelled.
            shares: Shares,
            owner:  Link[Portfolio],
            limit:  Price
        )
        extends KL
        with SellLimitOrderOps
        with OrderAttrs
        
    // Operations
    
    case class StockPurchase(
            shares:  Shares,
            dollars: Dollars
        )
    
    trait OrderAttrs {
        val ticker: String
        val shares: Shares
        val limit: Price
        
        def userCancel()
    }
    
    case class StockHolding(ticker: String, shares: Shares, dollars: Dollars)
    
    def leagueSH(league: League): List[StockHolding] = (stockAssets.toList
        filter {sa: StockAsset => sa.owner.league ~~ league}
        groupBy (_.ticker) map { case (ticker, assetGroup) =>
            val shares = (assetGroup map (_.shares)).summ
            StockHolding(ticker, shares, shares*Stocks.lastTradePrice(ticker))
        } toList)

    // ref_158
    def allStockHoldings: List[StockHolding] = (stockAssets.toList
        groupBy (_.ticker) map { case (ticker, assetGroup) =>
            val shares = (assetGroup map (_.shares)).summ
            StockHolding(ticker, shares, shares*Stocks.lastTradePrice(ticker))
        } toList)
    
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
    
    // ref_569
    trait PortfolioWithStocks {
        self: Portfolio =>
        
        // ref_937
        def myStockAssets = stockAssets where ('owner ~=~ self) toList
        
        // ref_407
        def haveTicker(ticker: String): Option[StockAsset] =
            stockAssets where ('owner ~=~ this) where ('ticker ~=~ ticker) headOption
        
        // Java interop
        def getMyStockAssets: java.util.List[StockAsset] = readDB(myStockAssets)
        
        // ref_666
        def howManyShares(ticker: String) = readDB {
            haveTicker(ticker) match {
                case Some(a) => a.shares
                case None    => Shares(0)
            }
        }
        
        // ref_873
        def howManyDollars(ticker: String) =
            howManyShares(ticker) * Stocks.lastTradePrice(ticker)
        
        // ref_850
        def userBuyStock(ticker: String, shares: Shares) =
            editDB(buyStock(ticker, shares))
        
        def userBuyStock(ticker: String, dollars: Dollars) =
            editDB(buyStock(ticker, dollars))
        
        // ref_620
        def userSellStock(ticker: String, shares: Shares) =
            editDB(sellStock(ticker, shares))
        
        def userSellStock(ticker: String, dollars: Dollars) =
            editDB(sellStock(ticker, dollars))
        
        // ref_306
        def userSellAll(ticker: String) = editDB(sellAll(ticker))
        
        // ref_184
        def userMakeBuyLimitOrder(ticker: String, shares: Shares, limit: Price) = editDB {
            makeBuyLimitOrder(ticker, shares, limit)
        }
        
        // ref_939
        def userMakeSellLimitOrder(ticker: String, shares: Shares, limit: Price) = editDB {
            makeSellLimitOrder(ticker, shares, limit)
        }
        
        // ref_734
        def myBuyLimitOrders = buyLimitOrders where ('owner ~=~ this) toList
        
        // ref_680
        def mySellLimitOrders = sellLimitOrders where ('owner ~=~ this) toList
        
        // There could be multiple sources of margin. Right now we have only this one
        // ref_224
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
                // ref_745
                trades <- processTradeables(buyers, okShares)
                dollars = Dollars((trades map (_.dollars.dollars)).sum)
                _      <- sellEach(trades)
                _      <- sellAll
                _      <- this update (t => t copy (cash=t.cash+dollars))
            }
            yield ()
        }
        
        private def processTradeables
            (tradeables: List[Tradeable], shares: Shares)
            : Transaction[List[Trade]] =
        {
            processTradeables(tradeables, shares, Nil)
        }
        
        // This is a recursive monadic function I'M SORRY
        private def processTradeables
            (tradeables: List[Tradeable], sharesRemaining: Shares, trades: List[Trade])
            : Transaction[List[Trade]] =
        {
            if (sharesRemaining <= Shares(0)) {
                Transaction(trades)
            }
            else tradeables match {
                // Maybe we should queue this as some kind of order?
                // There are reasons I don't want to do that.
                // ref_478
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
        
        private[model] def buyAsMuchAsYouCan(ticker: String, shares: Shares, limit: Price) = {
            val sellers = sellersFor(ticker) filter (_.price <= limit)
            val availableShares = (sellers map (_.available)).summ
            
            if (availableShares >= shares)
                for {
                    _ <- buyStock(ticker, shares)
                }
                yield Shares(0)
            else {
                val buy =
                    if (availableShares > Shares(0)) buyStock(ticker, availableShares)
                    else Transaction(())
                    
                val remaining = shares - availableShares
                
                for {
                    _ <- buy
                }
                yield shares - availableShares
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
        
        private[model] def creditShares(ticker: String, shares: Shares) = {
            haveTicker(ticker) match {
                case None => StockAsset(ticker=ticker, shares=shares, owner=this,
                    notifiedPrice=Price(0), lastDividendDate=new DateTime, totalDividends=Dollars(0)).insert
                case Some(asset) =>
                    asset update (a => a copy (shares=a.shares+shares))
            }
        }
        
        private[model] def debitShares(ticker: String, shares: Shares) = {
            haveTicker(ticker) match {
                case None =>
                    // Let's just pretend this never happened
                    Transaction(())
                case Some(asset) =>
                    if (shares >= asset.shares) asset.delete
                    else asset update (a => a copy (shares=a.shares-shares))
            }
        }
    }
    
    trait BuyLimitOrderOps {
        self: BuyLimitOrder =>
        
        def userCancel() { editDB(this.refetch.cancel) }
        
        private[model] def cancel() =
            for {
                // Delete the order,
                _ <- this.delete
                // Refund their margin
                _ <- this.owner update (p => p copy (cash=p.cash+setAside))
            }
            yield ()
    }
    
    trait SellLimitOrderOps {
        self: SellLimitOrder =>
            
        def userCancel() { editDB(this.refetch.cancel) }
        
        private[model] def cancel() =
            for {
                // Give the shares back
                _ <- this.owner creditShares (ticker, shares)
                // Delete the order
                _ <- this.delete
            }
            yield ()
    }
    
    trait Tradeable {
        val name: String
        val available: Shares
        val price: Price
        
        private[model] def satisfyPartially(shares: Shares): Transaction[Dollars]
        private[model] def satisfyCompletely = satisfyPartially(available)
    }
    
    private class AutomaticTrader(ticker: String, premium: Scale, targetDollars: Dollars,
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
        }
    }
    
    private var automaticSellers = MMap[String,List[AutomaticTrader]]()
    private var automaticBuyers  = MMap[String,List[AutomaticTrader]]()
    
    private def initAutomaticBuyers(ticker: String) = List(
        new AutomaticTrader(ticker, Scale("1.00"), Dollars(20000), "Des. Market Maker #3", true),
        new AutomaticTrader(ticker, Scale(".97"), Dollars(13000), "Des. Market Maker #7", true),
        new AutomaticTrader(ticker, Scale(".93"), Dollars( 8000), "Des. Market Maker #12", true)
    )
    
    private def initAutomaticSellers(ticker: String) = List(
        new AutomaticTrader(ticker, Scale("1.00"), Dollars(20000), "Des. Market Maker #3", true),
        new AutomaticTrader(ticker, Scale("1.04"), Dollars(13000), "Des. Market Maker #7", true),
        new AutomaticTrader(ticker, Scale("1.07"), Dollars( 8000), "Des. Market Maker #12", true)
    )
    
    def sellersFor(ticker: String): List[Tradeable] = {
        val auto = automaticSellers.getOrElseUpdate(ticker, {
            initAutomaticSellers(ticker)
        })
        
        (
           (auto map (_.makeTradeable)) ++ userSellers(ticker)
        ) filter (_.available>Shares(0)) sortBy (_.price)
    }
    
    def buyersFor(ticker: String): List[Tradeable] = {
        val auto = automaticBuyers.getOrElseUpdate(ticker,
            initAutomaticBuyers(ticker))
        
        (
           (auto map (_.makeTradeable)) ++ userBuyers(ticker)
        ) filter (_.available>Shares(0)) sortBy (- _.price)
    }
    
    private case class Trade(shares: Shares, dollars: Dollars) {
        val price = dollars / shares
    }
    
    private def userSellers(ticker: String): List[Tradeable] =
        (sellLimitOrders where ('ticker ~=~ ticker)).toList map { order =>
            new Tradeable {
                val name      = order.owner.name
                val available = order.shares
                val price     = order.limit
                
                def satisfyPartially(shares: Shares) = {
                    val remove =
                        if (shares >= order.shares) order.delete
                        else order update (o => o copy (shares=o.shares-shares))
                        
                    // ref_325
                    val dollars = shares*price
                        
                    for {
                        _ <- remove
                        _ <- order.owner update (p => p copy (cash=p.cash+dollars))
                    }
                    yield dollars
                }
            }
        }
    
    private def userBuyers(ticker: String): List[Tradeable] =
        (buyLimitOrders where ('ticker ~=~ ticker)).toList map { order =>
            new Tradeable {
                val name      = order.owner.name
                val available = order.shares
                val price     = order.limit
                
                def satisfyPartially(shares: Shares) = {
                    val dollars = shares*price
                    val margin = order.setAside
                    val nextMargin = margin - dollars
                    
                    if (shares >= order.shares)
                        for {
                            _ <- order.delete
                            _ <- order.owner update (p => p copy (cash=p.cash+nextMargin))
                        }
                        yield dollars
                        
                    else
                        for {
                            _ <- order update (o => o copy (shares=o.shares-shares))
                            _ <- order update (o => o copy (setAside=o.setAside-dollars))
                        }
                        yield dollars
                }
            }
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

