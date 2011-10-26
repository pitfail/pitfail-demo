
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
    def stockPrice(ticker: String): Price = {
        val stock = Stock(ticker)
        val quote = StockPriceSource.getQuotes(Seq(stock)).head
        quote.price
    }
}
