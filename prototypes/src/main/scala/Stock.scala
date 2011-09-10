package com.github.pitfail

import java.io.IOException
import java.net.{HttpURLConnection,URL,URLEncoder}
import org.joda.time.{DateTime,Duration}
import net.liftweb.json.{DefaultFormats,JsonParser,MappingException}
import scala.math.BigDecimal
import scala.collection.mutable.{Map => MMap}

class NoSuchSymbolException(val symbol: String) extends Exception(
  "There is no stock with ticker symbol '%s'".format(symbol))

class ExchangeMismatchException(val expectedExchange: String,
                                val actualExchange: String) extends Exception(
  "Expected exchange '%s'; got '%s'.".format(expectedExchange, actualExchange))

class DatabaseException(message: String) extends Exception(message)

class YahooStockDatabase(queryService: QueryService) extends StockDatabase {
  def getStock(exchange: String, symbol: String): Stock = {
    val url: URL = buildURL("http://query.yahooapis.com/v1/public/yql", Map(
      "q"      -> buildYQL(symbol),
      "format" -> "json",
      "env"    -> "store://datatables.org/alltableswithkeys"
    ), "ASCII")

    try {
      val response = queryService.query(url)

      implicit val formats = DefaultFormats
      val responseRoot = JsonParser.parse(response)
      val responseQuote = responseRoot\"query"\"results"\"quote"
      val responseExchange = (responseQuote\"StockExchange").extract[String]
      val responseSymbol = (responseQuote\"Symbol").extract[String]
      val responsePrice  = BigDecimal((responseQuote\"LastTradePriceOnly").extract[String])

      if (exchange != responseExchange) {
        throw new ExchangeMismatchException(exchange, responseExchange)
      } else if (symbol != responseSymbol) {
        throw new NoSuchSymbolException(symbol)
      }

      val updateTime = new DateTime()
      new Stock(exchange, symbol, responsePrice, updateTime)
    } catch {
      case ex: IOException =>
        throw new DatabaseException("Yahoo Finance query failed.")

      case ex: JsonParser.ParseException =>
        throw new DatabaseException("Yahoo Finance returned invalid JSON.")

      case ex: MappingException =>
        throw new DatabaseException("Yahoo Finance returned JSON with unexpected structured.")

      case ex: NumberFormatException =>
        throw new DatabaseException("Yahoo Finance returned an invalid stock price.")
    }
  }

  // Restrict exchange and symbol to be alphabetic.
  private def buildYQL(symbol: String) =
    "SELECT StockExchange,Symbol,LastTradePriceOnly from yahoo.finance.quotes where symbol in ('"+symbol+"')"

  private def buildURL(baseUrl: String,
               params: Iterable[(String, String)],
               encoding: String): URL = {
    val queryString = params map {
      case (key, value) =>
        (URLEncoder.encode(key, encoding) + "=" + URLEncoder.encode(value, encoding))
    }
      new URL(baseUrl + "?" + queryString.mkString("&"))
    }
}

class CachedStockDatabase(database: StockDatabase, timeout: Duration) extends StockDatabase {
  val symbols: MMap[(String, String), (BigDecimal, DateTime)] = MMap()

  def getStock(exchange: String, symbol: String): Stock =
    (symbols get (exchange, symbol) match {
      case Some((price: BigDecimal, updateTime: DateTime)) => {
        val stock = new Stock(exchange, symbol, price, updateTime)
        val expired = isExpired(updateTime)
        (Some(stock), expired)
      }
      case _ => (None, true)
    }) match {
      case (Some(stock), false) => stock
      case (_, true) => database.getStock(exchange, symbol)
    }

  private def isExpired(then: DateTime): Boolean =
    timeout.compareTo(new Duration(then, new DateTime())) > 0
}

class Stock(val exchange: String, val symbol: String, val price: BigDecimal, val updated: DateTime) {
  override val toString = exchange + ":" + symbol
}

trait StockDatabase {
  def getStock(exchange: String, symbol: String): Stock
}

