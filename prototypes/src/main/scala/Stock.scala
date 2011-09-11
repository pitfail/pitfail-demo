package com.github.pitfail

import org.joda.time.DateTime
import scala.math.BigDecimal

case class Stock(val exchange: String, val symbol: String)

case class Quote(val stock: Stock, val price: BigDecimal, val updateTime: DateTime)

object Quote {
  implicit def quoteToStock(quote: Quote): Stock = quote.stock
}
