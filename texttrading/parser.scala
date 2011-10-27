
package texttrading

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.math._

import model.{Shares,Dollars}

object parser extends JavaTokenParsers {

    def parseAction(text: String) = action(text)

    lazy val action = opt(tags) ~ ( buy | sell | sellAll | showPortfolio ) ~ opt(tail) ^^ {
        case _ ~ action ~ _ => action
    }

    lazy val tags = """([@#]\S*)+"""r

    lazy val tail = """(\S+\s+)+"""r

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
    lazy val stockShares = amount ~ ("shares" ~ opt("of")) ~ ticker ^^ {
        case shares ~ _ ~ ticker => StockShares(ticker, Shares(shares))
    }

    lazy val stockVolume = dollarAmount ~ (opt("dollars") ~ "of") ~ ticker ^^ {
        case dollars ~ _ ~ ticker => StockDollars(ticker, Dollars(dollars))
    }

    lazy val showPortfolio = (
        opt( ( "show" ~ "me" ~ "my" ) )
        ~ ("""portfolio\S*"""r) ) ^^ {
        case _ ~ _ => Portfolio
    }

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

