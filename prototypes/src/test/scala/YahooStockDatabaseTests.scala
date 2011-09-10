package com.github.pitfail

import java.io.IOException
import java.net.URL
import org.joda.time.{DateTime,Duration}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

class YahooStockDatabaseTests extends FunSuite with ShouldMatchers {
  val validQueryResponse = """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}"""

  test("getStock: Queries correct URL.") {
    val queryService = new MockQueryService((url) => {
      url.getProtocol() should equal ("http")
      url.getPort() should (equal (-1) or equal (80))
      url.getHost() should equal ("query.yahooapis.com")
      url.getPath() should equal ("/v1/public/yql")

      HttpQueryService.parseQuery(url.getQuery()) should equal(Map(
        "q"      -> "SELECT StockExchange,Symbol,LastTradePriceOnly from yahoo.finance.quotes where symbol in ('MSFT')",
        "format" -> "json",
        "env"    -> "store://datatables.org/alltableswithkeys"
      ))
      validQueryResponse
    })

    val database = new YahooStockDatabase(queryService)
    val stock = database.getStock("NasdaqNM", "MSFT")
  }

  test("getStock: Returns stock with correct attributes.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)

    val stock = database.getStock("NasdaqNM", "MSFT")

    stock.exchange should equal ("NasdaqNM")
    stock.symbol should equal ("MSFT")
    stock.price should equal (BigDecimal("25.74"))
  }

  test("getStock: Throws when stock exchange is mismatch.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)

    val ex = evaluating { database.getStock("WrongExchange", "MSFT") } should produce [ExchangeMismatchException]
    ex.expectedExchange should equal ("WrongExchange")
    ex.actualExchange should equal ("NasdaqNM")
  }

  test("getStock: Throws when stock is missing in response.") {
    val queryService = new MockQueryService((_) => validQueryResponse)
    val database = new YahooStockDatabase(queryService)

    val ex = evaluating { database.getStock("NasdaqNM", "FAKESYMBOL") } should produce [NoSuchSymbolException]
    ex.symbol should equal ("FAKESYMBOL")
  }

  test("getStock: Throws when response is not HTTP 200.") {
    val queryService = new MockQueryService((_) => throw new IOException("Fake Exception"))
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getStock("NasdaqNM", "MSFT") } should produce [DatabaseException]
  }

  test("getStock: Throws when response is not JSON.") {
    val queryService = new MockQueryService((_) => "{ Not Valid JSON ]")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getStock("NasdaqNM", "MSFT") } should produce [DatabaseException]
  }

  test("getStock: Throws when response is missing symbol.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getStock("NasdaqNM", "MSFT") } should produce [DatabaseException]
  }

  test("getStock: Throws when response is missing exchange.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","Symbol":"MSFT"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getStock("NasdaqNM", "MSFT") } should produce [DatabaseException]
  }

  test("getStock: Throws when response is missing price.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getStock("NasdaqNM", "MSFT") } should produce [DatabaseException]
  }

  test("getStock: Throws when price is not a decimal number.") {
    val queryService = new MockQueryService((_) => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"NotADecimal","Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}""")
    val database = new YahooStockDatabase(queryService)

    evaluating { database.getStock("NasdaqNM", "MSFT") } should produce [DatabaseException]
  }

  private class MockQueryService(callback: URL => String) extends QueryService {
    def query(url: URL): String = callback(url)
  }
}

class CachedStockDatabaseTests extends FunSuite with ShouldMatchers {
  test("constructor: Throws when database is null.") {
    val timeout = new Duration(1000)
    evaluating { new CachedStockDatabase(null, timeout) } should produce [NullPointerException]
  }

  test("constructor: Throws when timeout is null.") {
    val database = new MockStockDatabase((exchange, symbol) => throw new UnsupportedOperationException())
    evaluating { new CachedStockDatabase(database, null) } should produce [NullPointerException]
  }

  test("getStock: Queries if not in cache.") {
    // Arrange:
    var queried: Boolean = false
    val expectedStock = new Stock("NasdaqNM", "MSFT", BigDecimal("1.23"), new DateTime())
    val database = new MockStockDatabase((exchange, symbol) => {
      symbol should equal (expectedStock.symbol)
      exchange should equal (expectedStock.exchange)
      queried = true
      expectedStock
    })
    val cache = new CachedStockDatabase(database, new Duration(1))

    // Act:
    val actualStock = cache.getStock("NasdaqNM", "MSFT")

    // Assert:
    queried should equal (true)
    actualStock should equal (expectedStock)
  }

  test("getStock: Queries if cache is expired.") {
    // Arrange:
    var queryCount: Int = 0
    val expectedStock = new Stock("NasdaqNM", "MSFT", BigDecimal("1.23"), new DateTime(0L))
    val database = new MockStockDatabase((exchange, symbol) => {
      symbol should equal (expectedStock.symbol)
      exchange should equal (expectedStock.exchange)
      queryCount += 1
      expectedStock
    })
    val cache = new CachedStockDatabase(database, Duration.ZERO)

    // Act:
    val actualStock1 = cache.getStock("NasdaqNM", "MSFT")
    val actualStock2 = cache.getStock("NasdaqNM", "MSFT")

    // Assert:
    queryCount should equal (2)
    actualStock1 should equal (expectedStock)
    actualStock2 should equal (expectedStock)
  }

  test("getStock: Uses cache if not expired.") {
    // Arrange:
    var queryCount: Int = 0
    val expectedStock = new Stock("NasdaqNM", "MSFT", BigDecimal("1.23"), new DateTime())
    val database = new MockStockDatabase((exchange, symbol) => {
      symbol should equal ("MSFT")
      exchange should equal ("NasdaqNM")
      queryCount += 1
      expectedStock
    })
    val cache = new CachedStockDatabase(database, new Duration(100000))

    // Act:
    val actualStock1 = cache.getStock("NasdaqNM", "MSFT")
    val actualStock2 = cache.getStock("NasdaqNM", "MSFT")

    // Assert:
    queryCount should equal (1)
    actualStock1 should equal (expectedStock)
    actualStock2 should equal (expectedStock)
  }

  private class MockStockDatabase(callback: (String, String) => Stock) extends StockDatabase {
    def getStock(exchange: String, symbol: String) = callback(exchange, symbol)
  }
}
