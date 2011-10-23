
package texttrading

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.math._

object parser extends JavaTokenParsers {
    
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

