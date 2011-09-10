package com.github.pitfail

import java.net.{HttpURLConnection,URL,URLEncoder}
import org.joda.time.{DateTime,Duration}
import net.liftweb.json.{DefaultFormats,JsonParser}
import scala.math.BigDecimal
import scala.collection.mutable.{Map => MMap}

class YahooStockDatabase(queryService: QueryService) extends StockDatabase {
  def getStock(exchange: String, symbol: String): Stock = {
    val url: URL = buildURL("http://query.yahooapis.com/v1/public/yql", Map(
      "q"      -> buildYQL(exchange, symbol),
      "format" -> "json",
      "env"    -> "store://datatables.org/alltableswithkeys"
    ), "ASCII")

    val response = queryService.query(url)

    implicit val formats = DefaultFormats
    val root = JsonParser.parse(response)
    val count = (root\"query"\"count").extract[Int]
    val quote = root\"query"\"results"\"quote"

    val price  = BigDecimal((quote\"LastTradePriceOnly").extract[String])
    val updateTime = new DateTime()
    new Stock(exchange, symbol, price, updateTime)
  }

  // Restrict exchange and symbol to be alphabetic.
  private def buildYQL(exchange: String, symbol: String) =
    "SELECT * FROM yahoo.finance.quoteslist WHERE symbol='"+exchange+":"+symbol+"'"

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

