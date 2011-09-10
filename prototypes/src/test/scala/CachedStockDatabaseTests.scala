package com.github.pitfail

import java.io.IOException
import java.net.URL
import org.joda.time.{DateTime,Duration}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

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
