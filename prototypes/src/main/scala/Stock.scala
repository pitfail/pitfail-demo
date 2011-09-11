package com.github.pitfail

import org.joda.time.DateTime
import scala.math.BigDecimal

case class Stock(val exchange: String, val symbol: String) {
  override val toString = "%s:%s".format(exchange, symbol)
}

case class Quote(val stock: Stock, val price: BigDecimal, val updateTime: DateTime) {
  override val toString = "%s=%s".format(stock.toString, price.toString)
}

object Quote {
  implicit def quoteToStock(quote: Quote): Stock = quote.stock
}
