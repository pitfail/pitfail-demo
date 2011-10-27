package stockdata

import java.io.IOException
import java.net.URL
import org.joda.time.{DateTime,Duration}
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

class YahooStockDatabaseTests extends Spec with ShouldMatchers {
  val validQueryResponse = """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}"""
  val testStock = Stock("MSFT")

  describe("getQuotes") {
    it("Yahoo Finance service responds") {
      var queryService = new HttpQueryService("GET")
      var database = new YahooStockDatabase(queryService)

      var quotes = database.getQuotes(Iterable(testStock))

      quotes.head.stock should equal (testStock)
      quotes.head.price should (be >= BigDecimal("0.00"))
    }

    it("no stocks yields no quotes") {
      val queryService = new MockQueryService(_ => fail())
      val database = new YahooStockDatabase(queryService)

      database.getQuotes(Iterable()) should equal (Iterable())
    }

    it("queries correct URL") {
      val queryService = new MockQueryService(url => {
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
      database.getQuotes(Iterable(testStock))
    }

    it("returns one stock with correct attributes") {
      val stock = TH.msft_stock
      val quote = TH.q1(price = BigDecimal("25.74"))

      val queryService = new MockQueryService(_ => validQueryResponse)
      val database = new YahooStockDatabase(queryService)

      database.getQuotes(Iterable(stock)) should equal(Iterable(quote))
    }

    it("returns multiple stocks with correct attributes") {
      val stock1 = TH.msft_stock
      val stock2 = TH.appl_stock
      val quote1 = TH.q1(price = BigDecimal("25.74"))
      val quote2 = TH.q2(price = BigDecimal("377.48"))

      val queryService = new MockQueryService(_ => """{"query":{"count":2,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":[{"quote":{"LastTradePriceOnly":"25.74","Symbol":"MSFT","StockExchange":"NasdaqNM"}},{"quote":{"LastTradePriceOnly":"377.48","Symbol":"AAPL","StockExchange":"NasdaqNM"}}]}}""")
      val database = new YahooStockDatabase(queryService)

      database.getQuotes(Iterable(stock1, stock2)) should equal (Iterable(quote1, quote2))
    }

    it("throws when stock is missing in response") {
      val queryService = new MockQueryService(_ => validQueryResponse)
      val database = new YahooStockDatabase(queryService)
      val stock = Stock("FAKESYMBOL")

      val ex = evaluating { database.getQuotes(Iterable(stock)) } should produce [NoSuchStockException]
      ex.stock should equal (stock)
    }

    it("throws when response is not HTTP 200") {
      val queryService = new MockQueryService(_ => throw new IOException("Fake Exception"))
      val database = new YahooStockDatabase(queryService)

      evaluating { database.getQuotes(Iterable(testStock)) } should produce [DatabaseException]
    }

    it("throws when response is not JSON") {
      val queryService = new MockQueryService(_ => "{ Not Valid JSON ]")
      val database = new YahooStockDatabase(queryService)

      evaluating { database.getQuotes(Iterable(testStock)) } should produce [DatabaseException]
    }

    it("throws when response is missing symbol") {
      val queryService = new MockQueryService(_ => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","StockExchange":"NasdaqNM"}}}}""")
      val database = new YahooStockDatabase(queryService)

      evaluating { database.getQuotes(Iterable(testStock)) } should produce [DatabaseException]
    }

    it("throws when response is missing exchange") {
      val queryService = new MockQueryService(_ => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"25.74","Symbol":"MSFT"}}}}""")
      val database = new YahooStockDatabase(queryService)

      evaluating { database.getQuotes(Iterable(testStock)) } should produce [DatabaseException]
    }

    it("throws when response is missing price") {
      val queryService = new MockQueryService(_ => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}""")
      val database = new YahooStockDatabase(queryService)

      evaluating { database.getQuotes(Iterable(testStock)) } should produce [DatabaseException]
    }

    it("throws when price is not a decimal number") {
      val queryService = new MockQueryService(_ => """{"query":{"count":1,"created":"2011-09-10T06:36:19Z","lang":"en-US","results":{"quote":{"LastTradePriceOnly":"NotADecimal","Symbol":"MSFT","StockExchange":"NasdaqNM"}}}}""")
      val database = new YahooStockDatabase(queryService)

      evaluating { database.getQuotes(Iterable(testStock)) } should produce [DatabaseException]
    }
  }

  private class MockQueryService(callback: URL => String) extends QueryService {
    def query(url: URL): String = callback(url)
  }
}

// vim: set ts=2 sw=2 et:
