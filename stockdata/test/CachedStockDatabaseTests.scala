package stockdata

import java.io.IOException
import java.net.URL
import org.joda.time.{DateTime,Duration}
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

class CachedStockDatabaseTests extends Spec with ShouldMatchers {
  val testStock1 = TH.msft_stock
  val testStock2 = TH.appl_stock

  describe("constructor") {
    it("throws when database is null") {
      val timeout = new Duration(1000)
      evaluating { new CachedStockDatabase(null, timeout) } should produce [NullPointerException]
    }

    it("throws when timeout is null") {
      val database = new MockStockDatabase(_ => throw new UnsupportedOperationException())
      evaluating { new CachedStockDatabase(database, null) } should produce [NullPointerException]
    }
  }

  describe("getQuotes") {
    it("no stocks yields no quotes") {
      val database = new MockStockDatabase(stocks => fail())
      val cache = new CachedStockDatabase(database, new Duration(1))

      val quotes = cache.getQuotes(Iterable())
      quotes should equal (Iterable())
    }

    it("queries if not in cache") {
      // Arrange:
      var queried: Boolean = false
      val expectedQuote = TH.q1(price = BigDecimal("1.23"), time = new DateTime())
      val database = new MockStockDatabase((stocks: Iterable[Stock]) => {
        stocks should equal (Iterable(testStock1))
        queried = true
        Iterable(expectedQuote)
      })
      val cache = new CachedStockDatabase(database, new Duration(1))

      // Act:
      val actualQuotes = cache.getQuotes(Iterable(testStock1))

      // Assert:
      queried should equal (true)
      actualQuotes should equal (Iterable(expectedQuote))
    }

    it("queries if cache is expired") {
      // Arrange:
      var queryCount: Int = 0
      val expectedQuote = TH.q1(price = BigDecimal("1.23"), time = new DateTime(0L))
      val database = new MockStockDatabase((stocks: Iterable[Stock]) => {
        stocks should equal (Iterable(testStock1))
        queryCount += 1
        Iterable(expectedQuote)
      })
      val cache = new CachedStockDatabase(database, Duration.ZERO)

      // Act:
      val actualQuotes1 = cache.getQuotes(Iterable(testStock1))
      val actualQuotes2 = cache.getQuotes(Iterable(testStock1))

      // Assert:
      queryCount should equal (2)
      actualQuotes1 should equal (Iterable(expectedQuote))
      actualQuotes2 should equal (Iterable(expectedQuote))
    }

    it("uses cache if not expired") {
      // Arrange:
      var queryCount: Int = 0
      val expectedQuote = TH.q1(price = BigDecimal("1.23"), time = new DateTime())
      val database = new MockStockDatabase((stocks: Iterable[Stock]) => {
        queryCount match {
          case 0 => { stocks should equal (Iterable(testStock1)) }
          case 1 => { stocks should equal (Iterable()) }
          case _ => { fail() }
        }
        queryCount += 1
        Iterable(expectedQuote)
      })
      val cache = new CachedStockDatabase(database, new Duration(100000))

      // Act:
      val actualQuotes1 = cache.getQuotes(Iterable(testStock1))
      val actualQuotes2 = cache.getQuotes(Iterable(testStock1))

      // Assert:
      actualQuotes1 should equal (Iterable(expectedQuote))
      actualQuotes2 should equal (Iterable(expectedQuote))
    }

    it("mixes cache hits and misses") {
      // Arrange:
      val expectedQuote1 = TH.q1(price = BigDecimal("1.23"), time = new DateTime())
      val expectedQuote2 = TH.q2(price = BigDecimal("1.23"), time = new DateTime())
      val quoteMap = Map(testStock1 -> expectedQuote1, testStock2 -> expectedQuote2)
      var queryCount: Int = 0

      val database = new MockStockDatabase((stocks: Iterable[Stock]) => {
        queryCount match {
          case 0 => { stocks should equal (Iterable(testStock1)) }
          case 1 => { stocks should equal (Iterable(testStock2)) }
          case _ => fail()
        }
        queryCount += 1

        stocks map quoteMap
      })
      val cache = new CachedStockDatabase(database, new Duration(100000))

      // Act:
      val actualQuotes1 = cache.getQuotes(Iterable(testStock1))
      val actualQuotes2 = cache.getQuotes(Iterable(testStock1, testStock2))

      // Assert:
      actualQuotes1 should equal (Iterable(expectedQuote1))
      actualQuotes2 should equal (Iterable(expectedQuote1, expectedQuote2))
    }
  }

  private class MockStockDatabase(callback: Iterable[Stock] => Iterable[Quote]) extends StockDatabase {
    def getQuotes(stock: Iterable[Stock]): Iterable[Quote] = callback(stock)
  }
}

// vim: set ts=2 sw=2 et:
