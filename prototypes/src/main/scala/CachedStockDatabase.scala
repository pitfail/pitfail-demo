package com.github.pitfail

import org.joda.time.{DateTime,Duration}
import scala.math.BigDecimal
import scala.collection.mutable.{Map => MMap}

class CachedStockDatabase(database: StockDatabase, timeout: Duration) extends StockDatabase {
  val symbols: MMap[(String, String), (BigDecimal, DateTime)] = MMap()

  if (database == null)
    throw new NullPointerException("Database must be non-null.")

  if (timeout == null) {
    throw new NullPointerException("Timeout must be non-null.")

  def getStock(exchange: String, symbol: String): Stock =
    (symbols get (exchange, symbol) match {
      case Some((price: BigDecimal, updateTime: DateTime)) => {
        if (isExpired(updateTime))
          None
        else
          Some((price, updateTime))
      }
      case _ => None
    }) match {
      case Some((price: BigDecimal, updateTime: DateTime)) =>
        new Stock(exchange, symbol, price, updateTime)
      case None => {
        val stock = database.getStock(exchange, symbol)
        symbols((exchange, symbol)) = (stock.price, stock.updated)
        stock
      }
    }

  private def isExpired(then: DateTime): Boolean =
    timeout.compareTo(new Duration(then, new DateTime())) < 0
}
