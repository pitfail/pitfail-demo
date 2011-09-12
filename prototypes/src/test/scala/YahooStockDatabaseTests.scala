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

  test("getQuotes: Yahoo Finance service responds.") {
    var queryService = new HttpQueryService("GET")
    var database = new YahooStockDatabase(queryService)

    var quotes = database.getQuotes(List(testStock))

    quotes.head.stock should equal (testStock)
    quotes.head.price should (be >= BigDecimal("0.00"))
  }

  test("getQuotes: Queries correct URL.") {
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
    val stock = database.getQuotes(List(testStock))
  }

  test("getQuotes: Returns stock with correct attributes.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)

    val quotes = database.getQuotes(List(testStock))

    quotes.head.stock should equal (testStock)
    quotes.head.price should equal (BigDecimal("25.74"))
  }

  test("getQuotes: Throws when stock exchange is mismatch.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)
    val stock = Stock("WrongExchange", "MSFT")

    val ex = evaluating { database.getQuotes(List(stock)) } should produce [NoSuchStockException]
    ex.stock should equal (stock)
  }

  test("getQuotes: Throws when stock is missing in response.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)
    val stock = Stock("NasdaqNM", "FAKESYMBOL")

    val ex = evaluating { database.getQuotes(List(stock)) } should produce [NoSuchStockException]
    ex.stock should equal (stock)
  }

  test("getQuotes: Throws when response is not HTTP 200.") {
    val queryService = new MockQueryService((_) => throw new IOException("Fake Exception"))
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuotes(List(testStock)) } should produce [DatabaseException]
  }

  test("getQuotes: Throws when response is not JSON.") {
    val queryService = new MockQueryService((_) => "{ Not Valid JSON ]")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuotes(List(testStock)) } should produce [DatabaseException]
  }

  test("getQuotes: Throws when response is missing symbol.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuotes(List(testStock)) } should produce [DatabaseException]
  }

  test("getQuotes: Throws when response is missing exchange.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","Symbol":"MSFT"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuotes(List(testStock)) } should produce [DatabaseException]
  }

  test("getQuotes: Throws when response is missing price.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuotes(List(testStock)) } should produce [DatabaseException]
  }

  test("getQuotes: Throws when price is not a decimal number.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"NotADecimal","Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getQuotes(List(testStock)) } should produce [DatabaseException]
  }

  private class MockQueryService(callback: URL => String) extends QueryService {
    def query(url: URL): String = callback(url)
  }
}
