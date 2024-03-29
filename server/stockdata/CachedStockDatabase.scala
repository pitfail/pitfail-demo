
// Written by: Michael Koval

package stockdata

import org.joda.time.{DateTime,Duration}
import scala.math.BigDecimal
import scala.collection.mutable.{Map => MMap}

class CachedStockDatabase(database: StockDatabase, timeout: Duration) extends StockDatabase {
  val cache: MMap[Stock, Quote] = MMap()

  if (database == null)
    throw new NullPointerException("Database must be non-null.")

  if (timeout == null)
    throw new NullPointerException("Timeout must be non-null.")

  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] = {
    val now = new DateTime()

    val (cached, missing) = stocks partition { hasQuote(_, now) }

    val new_quotes =
      if (!missing.isEmpty)
        updateQuotes(missing)
      else
        Iterable[Quote]()

    val cached_quotes = cached map { getCachedQuote(_, now) get }

    cached_quotes ++ new_quotes
  }

  private def hasQuote(stock : Stock, now: DateTime): Boolean = {
    getCachedQuote(stock, now) match {
      case Some(quote: Quote) => true
      case None               => false
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
