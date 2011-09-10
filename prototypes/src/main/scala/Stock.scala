package com.github.pitfail

import org.joda.time.DateTime
import scala.math.BigDecimal

case class Stock(val exchange: String,
                 val symbol: String,
                 val price: BigDecimal,
                 val updated: DateTime)
