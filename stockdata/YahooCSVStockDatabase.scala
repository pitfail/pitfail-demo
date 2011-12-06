
package stockdata

import java.io.IOException
import java.net.URL

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import model.{Dollars, Shares, Price, Scale}

import net.liftweb.common.Loggable

class YahooCSVStockDatabase(queryService: QueryService) extends StockDatabase with Loggable {
  private val flags = List(
    "s",  // Symbol
    "x",  // StockExchange
    "l1", // LastTradePriceOnly
    "d1", // LastTradeDate
    "t1", // LastTradeTime
    "n",  // Name
    "o",  // Open
    "h",  // Days High
    "g",  // Days Low
    "p2", // Change in Percent
    "d",  // Dividend per Share
    "b",  // Bid
    "a"   // Ask
  )

  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] = {
    if (stocks.isEmpty) return Iterable()

    val queryString = HttpQueryService.buildQuery(Map(
      "s" -> (stocks map { _.symbol }).mkString(" "),
      "f" -> flags.mkString("")
    ), "UTF-8")

    val response = try {
      queryService.query(new URL("http://finance.yahoo.com/d/quotes.csv?" + queryString))
    } catch {
      case ex: IOException =>
        throw new DatabaseException("Yahoo Finance CSV query failed.", ex)
    }

    try { 
      response.split("$").init map { line =>
        val stuff = line.split("\\,")
        /*
          case List(symbol, exchange, price, date,
                    time, company, openPrice,
                    highPrice, lowPrice, percentChange,
                    dividendPerShare) =>
          */
         val price = Price(stuff(2))
         val bidPrice = try Price(stuff(11)) catch {
             case e: NumberFormatException => price * Scale("0.95")
         }
         val askPrice = try Price(stuff(12)) catch {
             case e: NumberFormatException => price * Scale("1.05")
         }
         
          Quote(
            Stock(parseQuotedString(stuff(0))),
            parseQuotedString(stuff(1)),
            parseQuotedString(stuff(5)),
            price,
            new DateTime(),
            QuoteInfo(
              tryParsePercent(stuff(9)),
              tryParseNumber(stuff(6)),
              tryParseNumber(stuff(8)),
              tryParseNumber(stuff(7)),
              tryParseNumber(stuff(10))
            ),
            bidPrice,
            askPrice
          )
//List("MSFT", "NasdaqNM", 26.915, "10/28/2011", "2:35pm", "Microsoft Corpora", 27.10, 27.19, 26.79, "-1.23%", 0.64)
      }
    } catch {
        case ex: Throwable =>
          logger.error("CSV Query Failed", ex)
          throw new DatabaseException("Yahoo Finance CSV query failed.", ex)
    }
  }

  private def parseQuotedString(str: String): String =
    str.stripPrefix("\"").stripSuffix("\"")

  private def tryParsePercent(str: String): Option[BigDecimal] =
    tryParseNumber(parseQuotedString(str).stripSuffix("%"))

  private def tryParseNumber(str: String): Option[BigDecimal] = {
    try {
      Some(BigDecimal(str))
    } catch {
      case _ => None
    }
  }
}


