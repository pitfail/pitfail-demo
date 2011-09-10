package com.github.pitfail

import java.io.IOException
import java.net.URL
import org.joda.time.DateTime
import net.liftweb.json.{DefaultFormats,JsonParser,MappingException}
import scala.math.BigDecimal

class YahooStockDatabase(queryService: QueryService) extends StockDatabase {
  def getStock(exchange: String, symbol: String): Stock = {
    val queryString = HttpQueryService.buildQuery(Map(
      "q"      -> buildYQL(symbol),
      "format" -> "json",
      "env"    -> "store://datatables.org/alltableswithkeys"
    ), "ASCII")
    val url = new URL("http://query.yahooapis.com/v1/public/yql?" + queryString)

    try {
      val response = queryService.query(url)

      implicit val formats = DefaultFormats
      val responseRoot = JsonParser.parse(response)
      val responseQuote = responseRoot\"query"\"results"\"quote"
      val responseExchange = (responseQuote\"StockExchange").extract[String]
      val responseSymbol = (responseQuote\"Symbol").extract[String]
      val responsePrice  = BigDecimal((responseQuote\"LastTradePriceOnly").extract[String])

      if (exchange != responseExchange)
        throw new ExchangeMismatchException(exchange, responseExchange)

      if (symbol != responseSymbol)
        throw new NoSuchSymbolException(symbol)

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
}


