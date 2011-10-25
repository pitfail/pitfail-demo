
package stockdata

import org.joda.time.DateTime
import model._
import model.Schema._

case class Stock(symbol: String) {
  override val toString = symbol
}

case class Quote(stock: Stock,
                 exchange: String,
                 company: String,
                 price: Price,
                 updateTime: DateTime,
                 info: QuoteInfo) {
  override val toString = "%s=%s".format(stock.toString, price.toString)

  // TODO: Need to put some thought into this...
  override def equals(that: Any): Boolean =
    that match {
      case Quote(thatStock, thatExchange, _, thatPrice, thatTime, _) => (
           stock == thatStock
        && exchange == thatExchange
        && price == thatPrice
        && updateTime == thatTime)
      case _ => false
    }
}

case class QuoteInfo(percentChange: Option[BigDecimal],
                     openPrice: Option[BigDecimal],
                     lowPrice: Option[BigDecimal],
                     highPrice: Option[BigDecimal],
                     dividendShare: Option[BigDecimal])

object Quote {
  implicit def quoteToStock(quote: Quote): Stock = quote.stock
}

// vim: set ts=2 sw=2 et:
