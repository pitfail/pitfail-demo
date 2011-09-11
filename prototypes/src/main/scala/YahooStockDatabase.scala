package com.github.pitfail

import java.io.IOException
import java.net.URL
import org.joda.time.DateTime
import net.liftweb.json.{DefaultFormats,JsonParser,MappingException}
import scala.math.BigDecimal

class YahooStockDatabase(queryService: QueryService) extends StockDatabase {
  def getQuote(stock: Stock): Quote = {
    val queryString = HttpQueryService.buildQuery(Map(
      "q"      -> buildYQL(List(stock)),
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

      if (stock.exchange != responseExchange)
        throw new ExchangeMismatchException(stock.exchange, responseExchange)

      if (stock.symbol != responseSymbol)
        throw new NoSuchSymbolException(stock.symbol)

      val updateTime = new DateTime()
      new Quote(stock, responsePrice, updateTime)
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

  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] = null

  // Restrict exchange and symbol to be alphabetic.
  private def buildYQL(symbols: Iterable[Stock]) = (
    "SELECT StockExchange,Symbol,LastTradePriceOnly"
      + " FROM yahoo.finance.quotes"
      + " WHERE symbol in (" + (symbols map { "'%s'" format _.symbol }).mkString(",") + ")"
  )
}


