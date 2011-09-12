package com.github.pitfail

import java.io.IOException
import java.net.URL
import org.joda.time.DateTime
import net.liftweb.json.{DefaultFormats,JsonParser,MappingException}
import scala.math.BigDecimal

class YahooStockDatabase(queryService: QueryService) extends StockDatabase {
  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] = {
    val queryString = HttpQueryService.buildQuery(Map(
      "q"      -> buildYQL(stocks),
      "format" -> "json",
      "env"    -> "store://datatables.org/alltableswithkeys"
    ), "ASCII")
    val url = new URL("http://query.yahooapis.com/v1/public/yql?" + queryString)
    val now = new DateTime()

    val response = try {
      queryService.query(url)
    } catch {
      case ex: IOException =>
        throw new DatabaseException("Yahoo Finance query failed.")
    }

    implicit val formats = DefaultFormats
    val root = try {
      JsonParser.parse(response)
    } catch {
      case ex: JsonParser.ParseException =>
        throw new DatabaseException("Yahoo Finance returned invalid JSON.")
    }

    val quoteMap = try {
      val count = (root\"query"\"count").extract[Int] 

      // When the response contains only one quote the quote array is omitted.
      val quoteElements = if (count == 1)
          List(root\"query"\"results"\"quote")
        else
          (root\"query"\"results"\"quote").children

      (quoteElements map { (quoteElement) => {
        val quoteExchange = (quoteElement\"StockExchange").extract[String]
        val quoteSymbol = (quoteElement\"Symbol").extract[String]
        val quotePrice = (quoteElement\"LastTradePriceOnly").extract[String]
        val stock = Stock(quoteExchange, quoteSymbol)
        (stock, Quote(stock, BigDecimal(quotePrice), now))
      }}).toMap
    } catch {
      case ex: MappingException =>
        throw new DatabaseException("Yahoo Finance returned JSON with unexpected structured.")

      case ex: NumberFormatException =>
        throw new DatabaseException("Yahoo Finance returned an invalid stock price.")
    }

    stocks map { (stock) => {
      (quoteMap get (stock)) match {
        case Some(quote: Quote) => quote
        case None => throw new NoSuchStockException(stock)
    }}}
  }

  // Restrict exchange and symbol to be alphabetic.
  private def buildYQL(symbols: Iterable[Stock]) = (
    "SELECT StockExchange,Symbol,LastTradePriceOnly"
      + " FROM yahoo.finance.quotes"
      + " WHERE symbol in (" + (symbols map { "'%s'" format _.symbol }).mkString(",") + ")"
  )
}


