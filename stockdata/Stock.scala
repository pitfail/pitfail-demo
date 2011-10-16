package stockdata

import org.joda.time.DateTime
import scala.math.BigDecimal

case class Stock(exchange: String, symbol: String) {
  override val toString = "%s:%s".format(exchange, symbol)
}

case class Quote(stock: Stock, price: BigDecimal, updateTime: DateTime) {
  override val toString = "%s=%s".format(stock.toString, price.toString)

  override def equals(that: Any): Boolean =
    that match {
      case Quote(thatStock, thatPrice, _) => (stock == thatStock && price == thatPrice)
      case _ => false
    }
}

object Quote {
  implicit def quoteToStock(quote: Quote): Stock = quote.stock
}

// vim: set ts=2 sw=2 et:
