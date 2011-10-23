
package twitter-cmd

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.math._

object TweetParser extends JavaTokenParsers {
    def main(args: Array[String]) {
        val cmds = Seq(
            "buy $23 of MSFT",
            "buy 23 shares of MSFT",
            "sell MSFT",
            "your face"
        )

        cmds foreach { text =>
            println("%s:\n%s" format (text, action(text)) )
            println()
        }
    }

    sealed abstract class Action
    case class Buy(asset: StockAsset) extends Action
    case class Sell(asset: StockAsset) extends Action
    case class SellAll(ticker: String) extends Action

    sealed abstract class StockAsset
    case class StockShares(ticker: String, volume: BigDecimal) extends StockAsset
    case class StockVolume(ticker: String, volume: BigDecimal) extends StockAsset

    lazy val action = buy | sell | sellAll

    lazy val buy = "buy" ~ stockAsset ^^ {
        case _ ~ ass => Buy(ass)
    }

    lazy val sell = "sell" ~ stockAsset ^^ {
        case _ ~ ass => Sell(ass)
    }

    lazy val sellAll = "sell" ~ ticker ^^ {
        case _ ~ ticker => SellAll(ticker)
    }

    lazy val stockAsset = stockShares | stockVolume
    lazy val stockShares = (
          amount ~ "shares" ~ "of" ~ ticker ^^ {
              case shares ~ _ ~ _ ~ ticker => StockShares(ticker, shares)
          }
        | amount ~ "shares" ~ ticker ^^ {
              case shares ~ _ ~ ticker => StockShares(ticker, shares)
          }
    )
    lazy val stockVolume = (
          dollarAmount ~ "of" ~ ticker ^^ {
              case volume ~ _ ~ ticker => StockVolume(ticker, volume)
          }
        | dollarAmount ~ "dollars" ~ "of" ~ ticker ^^ {
              case volume ~ _ ~ _ ~ ticker => StockVolume(ticker, volume)
          }
    )

    lazy val amount = decimalNumber ^^ {
        case numText => BigDecimal(numText)
    }

    lazy val dollarAmount = (
          amount
        | "$" ~ amount ^^ {
            case _ ~ amount => amount
          }
    )

    lazy val ticker = ident

    implicit def toCSR(s: String): CharSequenceReader = new CharSequenceReader(s)
}

