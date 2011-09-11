package com.github.pitfail

import java.io.IOException
import java.net.URL
import org.joda.time.{DateTime,Duration}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

class YahooStockDatabaseTests extends FunSuite with ShouldMatchers {
  val validQueryResponse = """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}"""
  val testStock = Stock("NasdaqNM", "MSFT")

  test("getQuote: Yahoo Finance service responds.") {
    var queryService = new HttpQueryService("GET")
    var database = new YahooStockDatabase(queryService)

    var quote = database.getQuote(testStock)

    quote.stock should equal (testStock)
    quote.price should (be >= BigDecimal("0.00"))
  }

  test("getQuote: Queries correct URL.") {
    val queryService = new MockQueryService((url) => {
      url.getProtocol() should equal ("http")
      url.getPort() should (equal (-1) or equal (80))
      url.getHost() should equal ("query.yahooapis.com")
      url.getPath() should equal ("/v1/public/yql")

      HttpQueryService.parseQuery(url.getQuery(), "ASCII") should equal(Map(
        "q"      -> "SELECT StockExchange,Symbol,LastTradePriceOnly FROM yahoo.finance.quotes WHERE symbol in ('MSFT')",
        "format" -> "json",
        "env"    -> "store://datatables.org/alltableswithkeys"
      ))
      validQueryResponse
    })

    val database = new YahooStockDatabase(queryService)
    val stock = database.getQuote(testStock)
  }

  test("getQuote: Returns stock with correct attributes.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)

    val quote = database.getQuote(testStock)

    quote.stock should equal (testStock)
    quote.price should equal (BigDecimal("25.74"))
  }

  test("getQuote: Throws when stock exchange is mismatch.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)
    val stock = Stock("WrongExchange", "MSFT")

    val ex = evaluating { database.getQuote(stock) } should produce [NoSuchStockException]
    ex.stock should equal (stock)
  }

  test("getQuote: Throws when stock is missing in response.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)
    val stock = Stock("NasdaqNM", "FAKESYMBOL")

    val ex = evaluating { database.getQuote(stock) } should produce [NoSuchStockException]
    ex.stock should equal (stock)
  }

  test("getQuote: Throws when response is not HTTP 200.") {
    val queryService = new MockQueryService((_) => throw new IOException("Fake Exception"))
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuote(testStock) } should produce [DatabaseException]
  }

  test("getQuote: Throws when response is not JSON.") {
    val queryService = new MockQueryService((_) => "{ Not Valid JSON ]")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuote(testStock) } should produce [DatabaseException]
  }

  test("getQuote: Throws when response is missing symbol.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuote(testStock) } should produce [DatabaseException]
  }

  test("getQuote: Throws when response is missing exchange.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","Symbol":"MSFT"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuote(testStock) } should produce [DatabaseException]
  }

  test("getQuote: Throws when response is missing price.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuote(testStock) } should produce [DatabaseException]
  }

  test("getQuote: Throws when price is not a decimal number.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"NotADecimal","Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuote(testStock) } should produce [DatabaseException]
  }

  private class MockQueryService(callback: URL => String) extends QueryService {
    def query(url: URL): String = callback(url)
  }

  test("getQuotes: ???") {
    val queryService = new HttpQueryService("GET")
    val database = new YahooStockDatabase(queryService)

    database.getQuotes(List(Stock("NasdaqNM", "MSFT")))
    database.getQuotes(List(Stock("NasdaqNM", "MSFT"), Stock("NasdaqNM", "AAPL")))
  }
}
