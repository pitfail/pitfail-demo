package com.github.pitfail

trait StockDatabase {
  def getStock(exchange: String, symbol: String): Stock
}

class NoSuchSymbolException(val symbol: String) extends Exception(
  "There is no stock with ticker symbol '%s'".format(symbol))

class ExchangeMismatchException(val expectedExchange: String,
                                val actualExchange: String) extends Exception(
  "Expected exchange '%s'; got '%s'.".format(expectedExchange, actualExchange))

class DatabaseException(message: String) extends Exception(message)
