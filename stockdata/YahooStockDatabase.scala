
package stockdata

import java.io.IOException
import java.net.URL

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import net.liftweb.json.JsonAST.JValue

import net.liftweb.json.{DefaultFormats,JsonParser,MappingException}
import model.{Dollars, Shares, Price}

// Sorry, Mike, you can take this out later.
import net.liftweb.common.Loggable

class YahooStockDatabase(queryService: QueryService) extends StockDatabase with Loggable {
  private implicit val formats = DefaultFormats
  private val dateTimeFormat = DateTimeFormat.forPattern("M/d/yyyy h:mma")
  private val responseFields = List("Symbol", "StockExchange", "LastTradePriceOnly",
    "LastTradeDate", "LastTradeTime", "Name", "Open", "DaysHigh", "DaysLow",
    "ChangeinPercent", "DividendShare"
  )

  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] = {
    if (stocks.isEmpty) return Iterable()

    val queryString = HttpQueryService.buildQuery(Map(
      "q"      -> buildYQL(stocks),
      "format" -> "json",
      "env"    -> "store://datatables.org/alltableswithkeys"
    ), "ASCII")
    val url = new URL("http://query.yahooapis.com/v1/public/yql?" + queryString)
    val now = new DateTime()

    val response = try {
      queryService.query(url)
    } catch {
      case ex: IOException =>
        throw new DatabaseException("Yahoo Finance query failed.", ex)
    }

    val root = try {
      JsonParser.parse(response)
    } catch {
      case ex: JsonParser.ParseException =>
        throw new DatabaseException("Yahoo Finance returned invalid JSON.", ex)
    }

    val quoteMap = try {
      val count = (root\"query"\"count").extract[Int] 

      // When the response contains only one quote the quote array is omitted.
      val quoteElements = if (count == 1)
          List(root\"query"\"results"\"quote")
        else
          (root\"query"\"results"\"quote").children

      // DEVNOTE: Any fields referenced here must also be added to responseFields.
      (quoteElements map { (quoteElement) =>
        val stock = Stock((quoteElement\"Symbol").extract[String])
        val updateTime =
            try {
                dateTimeFormat.parseDateTime(
                    (quoteElement\"LastTradeDate").extract[String]
                    + " "
                    + (quoteElement\"LastTradeTime").extract[String]
                )
            } catch {
                case e: IllegalArgumentException =>
                    logger.error("Failed to parse date", e);
                    throw new NoSuchStockException(stock)
            }
        
        val price = Price((quoteElement\"LastTradePriceOnly").extract[String])
        val quote = Quote(
            stock      = stock,
            company    = (quoteElement\"Name").extract[String],
            exchange   = (quoteElement\"StockExchange").extract[String],
            price      = price,
            updateTime = new DateTime(),
            info = QuoteInfo(
                percentChange = Some(BigDecimal((quoteElement\"ChangeinPercent")
                    .extract[String].stripSuffix("%"))),
                dividendShare = tryExtractNumber(quoteElement\"DividendShare"),
                openPrice     = tryExtractNumber(quoteElement\"Open"),
                lowPrice      = tryExtractNumber(quoteElement\"DaysLow"),
                highPrice     = tryExtractNumber(quoteElement\"DaysHigh")
            ),
            bidPrice = price, // TODO: Make these accurate
            askPrice = price
        )
        
        (stock, quote)
      }).toMap
    } catch {
      case ex: MappingException =>
        throw new DatabaseException("Yahoo Finance returned JSON with unexpected structure.", ex)

      case ex: NumberFormatException =>
        throw new DatabaseException("Yahoo Finance returned an invalid stock price.", ex)
    }

    stocks map { (stock) => {
      (quoteMap get (stock)) match {
        case Some(quote: Quote) => quote
        case None =>
            logger.error("Failed to find " + stock)
            logger.error(response)
            logger.error(queryString)
            // FOR TESTING PURPOSES ONLY
            makeUpQuote(stock)
            //throw new NoSuchStockException(stock)
    }}}
  }

  private def makeUpQuote(stock: Stock) = {
      val price = Price(scala.math.random * 100)
      Quote(
          stock      ,
          exchange   = "PITFAIL",
          company    = "Enterprise Solutions Inc",
          price      = price,
          updateTime = new DateTime,
          info       = QuoteInfo(None, None, None, None, None),
          bidPrice   = price,
          askPrice   = price
      )
  }
  
  private def tryExtractNumber(field: JValue): Option[BigDecimal] = {
    try {
      Some(BigDecimal(field.extract[String]))
    } catch {
      case _ => None
    }
  }

  // Restrict exchange and symbol to be alphabetic.
  private def buildYQL(symbols: Iterable[Stock]) = (
    "SELECT " + responseFields.mkString(",") + " FROM yahoo.finance.quotes "
      + "WHERE symbol in (" + (symbols map { "'%s'" format _.symbol }).mkString(",") + ")"
  )
}


