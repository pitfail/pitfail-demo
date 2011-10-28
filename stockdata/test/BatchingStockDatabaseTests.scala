package stockdata

import org.joda.time.DateTime
import scala.math.BigDecimal

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class BatchingStockDatabaseTests extends Spec with ShouldMatchers {
  val testStock1 = TH.msft_stock
  val testStock2 = TH.appl_stock

  val testQuote1 = TH.q1(price = BigDecimal("25.12"))
  val testQuote2 = TH.q2(price = BigDecimal("350.34"))
  val testQuotes = Map(testStock1 -> testQuote1, testStock2 -> testQuote2)

  describe("constructor") {
    it("throws when database is null") {
      evaluating { new BatchingStockDatabase(null) } should produce [NullPointerException]
    }
  }

  describe("getQuotes") {
    it("lazily executes database query") {
      // Arrange:
      var callCount = 0
      val database = new StockDatabaseMock(stocks => {
        stocks.toSet should equal (Set(testStock1))
        callCount += 1
        Iterable(testQuote1)
      })
      val batcher = new BatchingStockDatabase(database)

      // Act:
      val lazyQuote = batcher.getQuote(testStock1)

      // Assert:
      callCount should equal (0)
      (lazyQuote: Quote) should equal (testQuote1)
      callCount should equal (1)
    }

    it("batches successive lazy queries") {
      // Arrange:
      var callCount = 0
      val database = new StockDatabaseMock(stocks => {
        stocks.toSet should equal (Set(testStock1, testStock2))
        callCount += 1
        Iterable(testQuote1, testQuote2)
      })
      val batcher = new BatchingStockDatabase(database)

      // Act:
      val lazyQuote1 = batcher.getQuote(testStock1)
      val lazyQuote2 = batcher.getQuote(testStock2)

      // Assert:
      callCount should equal (0)
      (lazyQuote1: Quote) should equal (testQuote1)
      (lazyQuote2: Quote) should equal (testQuote2)
      callCount should equal (1)
    }

    it("starts new batch after evaluation") {
      // Arrange:
      var callCount = 0
      val database = new StockDatabaseMock(stocks => {
        callCount += 1
        callCount match {
          case 1 => Iterable(testQuote1)
          case 2 => Iterable(testQuote1)
          case _ => fail()
        }
      })
      val batcher = new BatchingStockDatabase(database)

      // Act/Assert:
      val lazyQuote1a = batcher.getQuote(testStock1)
      (lazyQuote1a: Quote) should equal (testQuote1)
      callCount should equal (1)

      val lazyQuote1b = batcher.getQuote(testStock1)
      (lazyQuote1b: Quote) should equal (testQuote1)
      callCount should equal (2)

      lazyQuote1a should not equal (lazyQuote1b)
    }

    it("merges duplicate stocks in the same batch") {
      // Arrange:
      var callCount = 0
      val database = new StockDatabaseMock(stocks => {
        callCount += 1
        callCount match {
          case 1 => Iterable(testQuote1)
          case 2 => Iterable(testQuote1)
          case _ => fail()
        }
      })
      val batcher = new BatchingStockDatabase(database)

      // Act/Assert:
      val lazyQuote1a = batcher.getQuote(testStock1)
      val lazyQuote1b = batcher.getQuote(testStock1)

      (lazyQuote1a: Quote) should equal (testQuote1)
      (lazyQuote1b: Quote) should equal (testQuote1)
      callCount should equal (1)
    }
  }

  private class StockDatabaseMock(callback: Iterable[Stock] => Iterable[Quote]) extends StockDatabase {
    def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] =
      callback(stocks)
  }
}

// vim: set ts=2 sw=2 et:
