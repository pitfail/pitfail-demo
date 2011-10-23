
package pitfail

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class TweetParserTest extends Spec with ShouldMatchers {
  val t_tweets = List("@pitfail #buy $100 MSFT")

  describe("parse") {
    it("Does not process commands not destined to '@pitfail'") {
      TweetParser("@pitnotfail #buy $100 MSFT") should equal (None)
    }

    it("parses a simple buy order") {
      TweetParser("#buy $100 of MSFT") should equal (Buy(StockVolume("MSFT", 100)))
    }

    it("parses a simple sell order") {
      TweetParser("#sell $100 of MSFT") should equal (Sell(StockVolume("MSFT", 100)))
    }
  }

  describe("groupWithIx") {
    it("Groups and tracks indexes") {
      TweetParser.groupWithIx(List(1, 1, 2, 3)) should equal(
        Map(1 -> (2, Set(1, 0)),
             2 -> (1, Set(2)),
             3 -> (1, Set(3))
        )
      )
    }
  }
}
