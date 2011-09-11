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

  test("getQuote: Queries if not in cache.") {
    // Arrange:
    var queried: Boolean = false
    val expectedQuote = Quote(testStock, BigDecimal("1.23"), new DateTime())
    val database = new MockStockDatabase((stock: Stock) => {
      stock should equal (testStock)
      queried = true
      expectedQuote
    })
    val cache = new CachedStockDatabase(database, new Duration(1))

    // Act:
    val actualQuote = cache.getQuote(testStock)

    // Assert:
    queried should equal (true)
    actualQuote should equal (expectedQuote)
  }

  test("getQuote: Queries if cache is expired.") {
    // Arrange:
    var queryCount: Int = 0
    val expectedQuote = Quote(testStock, BigDecimal("1.23"), new DateTime(0L))
    val database = new MockStockDatabase((stock: Stock) => {
      stock should equal (testStock)
      queryCount += 1
      expectedQuote
    })
    val cache = new CachedStockDatabase(database, Duration.ZERO)

    // Act:
    val actualQuote1 = cache.getQuote(testStock)
    val actualQuote2 = cache.getQuote(testStock)

    // Assert:
    queryCount should equal (2)
    actualQuote1 should equal (expectedQuote)
    actualQuote2 should equal (expectedQuote)
  }

  test("getQuote: Uses cache if not expired.") {
    // Arrange:
    var queryCount: Int = 0
    val expectedQuote = Quote(testStock, BigDecimal("1.23"), new DateTime())
    val database = new MockStockDatabase((stock: Stock) => {
      stock should equal (testStock)
      queryCount += 1
      expectedQuote
    })
    val cache = new CachedStockDatabase(database, new Duration(100000))

    // Act:
    val actualQuote1 = cache.getQuote(testStock)
    val actualQuote2 = cache.getQuote(testStock)

    // Assert:
    queryCount should equal (1)
    actualQuote1 should equal (expectedQuote)
    actualQuote2 should equal (expectedQuote)
  }

  private class MockStockDatabase(callback: Stock => Quote) extends StockDatabase {
    def getQuote(stock: Stock): Quote = callback(stock)

    def getQuotes(stock: Iterable[Stock]): Iterable[Quote] = null
  }
}
