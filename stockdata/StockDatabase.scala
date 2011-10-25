package stockdata

trait StockDatabase {
  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote]
}

class NoSuchStockException(val stock: Stock) extends Exception(
  "There is no stock with ticker symbol '%s'.".format(stock.symbol))

class DatabaseException(
    message: String,
    source: Throwable
) extends Exception(message, source)

// vim: set ts=2 sw=2 et:
