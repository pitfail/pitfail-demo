
// Written by: Michael Koval

package stockdata

import scala.collection.mutable.{Map => MMap}

class BatchingStockDatabase(val database: StockDatabase) {
  private val pendingStocks: MMap[Stock, Long] = MMap()
  private val storedQuotes: MMap[Long, Quote] = MMap()
  private var previousId: Long = 0

  if (database == null)
    throw new NullPointerException("Database must be non-null.")

  def getQuote(stock: Stock): LazyQuote =
    (pendingStocks get stock) match {
      case Some(id) => LazyQuote(id)
      case None     => {
        previousId += 1
        pendingStocks += ((stock, previousId))
        LazyQuote(previousId)
      }
    }

  def getQuotes(stocks: Iterable[Stock]): Iterable[LazyQuote] =
    stocks map (stock => getQuote(stock))


  private def fetchPendingStock(id: Long): Quote = {
    // FIXME: This assumes that getQuotes preserves order.
    val quotes = database.getQuotes(pendingStocks.keys)

    (pendingStocks.values zip quotes) foreach (_ match {
      case (id, quote) => { storedQuotes(id) = quote }
    })
    pendingStocks.clear()

    (storedQuotes get id) match {
      case Some(stock) => stock
      case None        => null
    }
  }

  case class LazyQuote(private val id: Long)

  object LazyQuote {
    implicit def forceLazyQuote(lazyQuote: LazyQuote): Quote =
      (storedQuotes get lazyQuote.id) match {
        case Some(quote) => quote
        case None => fetchPendingStock(lazyQuote.id)
      }
  }
}

// vim: set ts=2 sw=2 et:
