package com.github.pitfail

import org.joda.time.{DateTime,Duration}
import scala.math.BigDecimal
import scala.collection.mutable.{Map => MMap}

class CachedStockDatabase(database: StockDatabase, timeout: Duration) extends StockDatabase {
  val stocks: MMap[Stock, Quote] = MMap()

  if (database == null)
    throw new NullPointerException("Database must be non-null.")

  if (timeout == null)
    throw new NullPointerException("Timeout must be non-null.")

  def getQuote(stock: Stock): Quote =
    (stocks get(stock)) match {
      case Some(quote: Quote) => {
        if (isExpired(quote.updateTime))
          updateQuote(stock)
        else
          quote
      }
      case None => updateQuote(stock)
    }

  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] = null

  private def updateQuote(stock: Stock): Quote = {
    val quote = database.getQuote(stock)
    stocks(stock) = quote
    quote
  }
  
  private def isExpired(then: DateTime): Boolean =
    timeout.compareTo(new Duration(then, new DateTime())) < 0
}
