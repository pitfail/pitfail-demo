package com.github.pitfail

import java.io.IOException
import java.net.URL
import org.joda.time.{DateTime,Duration}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

class CachedStockDatabaseTests extends FunSuite with ShouldMatchers {
  val testStock = Stock("NasdaqNM", "MSFT")

  test("constructor: Throws when database is null.") {
    val timeout = new Duration(1000)
    evaluating { new CachedStockDatabase(null, timeout) } should produce [NullPointerException]
  }

  test("constructor: Throws when timeout is null.") {
    val database = new MockStockDatabase((_) => throw new UnsupportedOperationException())
    evaluating { new CachedStockDatabase(database, null) } should produce [NullPointerException]
  }

  test("getQuotes: Queries if not in cache.") {
    // Arrange:
    var queried: Boolean = false
    val expectedQuote = Quote(testStock, BigDecimal("1.23"), new DateTime())
    val database = new MockStockDatabase((stocks: Iterable[Stock]) => {
      stocks should equal (List(testStock))
      queried = true
      List(expectedQuote)
    })
    val cache = new CachedStockDatabase(database, new Duration(1))

    // Act:
    val actualQuotes = cache.getQuotes(List(testStock))

    // Assert:
    queried should equal (true)
    actualQuotes should equal (List(expectedQuote))
  }

  test("getQuotes: Queries if cache is expired.") {
    // Arrange:
    var queryCount: Int = 0
    val expectedQuote = Quote(testStock, BigDecimal("1.23"), new DateTime(0L))
    val database = new MockStockDatabase((stocks: Iterable[Stock]) => {
      stocks should equal (List(testStock))
      queryCount += 1
      List(expectedQuote)
    })
    val cache = new CachedStockDatabase(database, Duration.ZERO)

    // Act:
    val actualQuotes1 = cache.getQuotes(List(testStock))
    val actualQuotes2 = cache.getQuotes(List(testStock))

    // Assert:
    queryCount should equal (2)
    actualQuotes1 should equal (List(expectedQuote))
    actualQuotes2 should equal (List(expectedQuote))
  }

  test("getQuotes: Uses cache if not expired.") {
    // Arrange:
    var queryCount: Int = 0
    val expectedQuote = Quote(testStock, BigDecimal("1.23"), new DateTime())
    val database = new MockStockDatabase((stocks: Iterable[Stock]) => {
      queryCount match {
        case 0 => { stocks should equal (List(testStock)) }
        case 1 => { stocks should equal (List()) }
        case _ => { fail() }
      }
      queryCount += 1
      List(expectedQuote)
    })
    val cache = new CachedStockDatabase(database, new Duration(100000))

    // Act:
    val actualQuotes1 = cache.getQuotes(List(testStock))
    val actualQuotes2 = cache.getQuotes(List(testStock))

    // Assert:
    actualQuotes1 should equal (List(expectedQuote))
    actualQuotes2 should equal (List(expectedQuote))
  }

  private class MockStockDatabase(callback: Iterable[Stock] => Iterable[Quote]) extends StockDatabase {
    def getQuotes(stock: Iterable[Stock]): Iterable[Quote] = callback(stock)
  }
}
