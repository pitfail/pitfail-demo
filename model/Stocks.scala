
package model

import Schema._
import scala.math.BigDecimal
import stockdata._
import org.joda.time.Duration

object StockPriceSource extends CachedStockDatabase(
    new YahooStockDatabase(new HttpQueryService("GET")),
    // TODO: This timeout should be moved to a configuration file.
    new Duration(1000 * 60 * 5)
)

// TODO: This needs to go.
object Stocks {
    case class StockShares(ticker: String, shares: Shares) {
        def price: Price = stockPrice(ticker)
        def value: Dollars = price * shares
    }

    object StockShares {
        def apply(ticker: String, dollars: Dollars): StockShares =
            StockShares(ticker, dollars / stockPrice(ticker))
    }
    
    // TODO: This is obviously wrong
    def stockPrice(ticker: String): Price = {
        val stock = Stock(ticker)
        val quote = StockPriceSource.getQuotes(Seq(stock)).head
        Price(quote.price)
    }
}
