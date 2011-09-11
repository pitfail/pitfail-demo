package com.github.pitfail

trait StockDatabase {
  def getQuote(stock: Stock): Quote
  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote]
}

class NoSuchStockException(val stock: Stock) extends Exception(
  "There is no stock with ticker symbol '%s' in the %s exchange.".format(stock.symbol, stock.exchange))

class DatabaseException(message: String) extends Exception(message)
