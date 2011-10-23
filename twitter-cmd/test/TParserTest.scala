package twitterCmd

import twitterCmd.TParser._

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class TParserTest extends Spec with ShouldMatchers {
  describe("action") {

    it ("Handles properly formed commands") {
      val cmds = Map(
        "buy $23 of MSFT" -> Buy(StockVolume("MSFT", 23)),
        "buy 23 shares of MSFT" -> Buy(StockShares("MSFT", 23)),
        "buy 23 MSFT shares" -> Buy(StockShares("MSFT", 23)),
        "sell MSFT" -> SellAll("MSFT")
      )

      cmds foreach { (cmd) =>
        val (text, result) = cmd
        TParser action text should equal result
      }
    }

    it ("Rejects improperly formed commands") {
      val cmds = Seq(
        "your face"
      )

      cmds foreach { text =>
        evaluating { TParser action text } should produce [Exception]
      }
    }
  }
}
