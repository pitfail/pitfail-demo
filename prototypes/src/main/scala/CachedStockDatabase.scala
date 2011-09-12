package com.github.pitfail

import org.joda.time.{DateTime,Duration}
import scala.math.BigDecimal
import scala.collection.mutable.{Map => MMap}

class CachedStockDatabase(database: StockDatabase, timeout: Duration) extends StockDatabase {
  val cache: MMap[Stock, Quote] = MMap()

  if (database == null)
    throw new NullPointerException("Database must be non-null.")

  if (timeout == null)
    throw new NullPointerException("Timeout must be non-null.")

  def getQuote(stock: Stock): Quote =
    getQuotes(List(stock)).head

  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] = {
    val now = new DateTime()
 
    // Split cached stocks from those that need refreshing.
    (stocks.foldLeft( (List[Quote](), List[Stock]()) )(
      (lists, stock) => {
        lists match {
          case (quotes: List[Quote], stocks: List[Stock]) => {
            getCachedQuote(stock, now) match {
              case Some(quote: Quote) => (quotes :+ quote, stocks)
              case None => (quotes, stocks :+ stock)
    }}}})) match {
      case (cachedQuotes: List[Quote], missingStocks: List[Stock]) => {
        val updatedQuotes = if (missingStocks.isEmpty) List() else updateQuotes(missingStocks)
        cachedQuotes ++ updatedQuotes
      }
    }
  }

  private def getCachedQuote(stock: Stock, now: DateTime): Option[Quote] =
    (cache get stock) match {
      case Some(quote: Quote) => {
        if (isExpired(quote, now))
          None
        else
          Some(quote)
      }
      case None => None
    }


  private def updateQuotes(stocks: Iterable[Stock]): Iterable[Quote] = {
    val quotes = database.getQuotes(stocks)
    quotes map { (quote) => {
      cache(quote.stock) = quote
      quote
    }}
  }
  
  private def isExpired(quote: Quote, now: DateTime): Boolean =
    timeout.compareTo(new Duration(quote.updateTime, now)) < 0
}
