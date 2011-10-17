package stockdata

import java.io.IOException
import java.net.URL

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import net.liftweb.json.{DefaultFormats,JsonParser,MappingException}
import scala.math.BigDecimal

class YahooStockDatabase(queryService: QueryService) extends StockDatabase {
  private val dateTimeFormat = DateTimeFormat.forPattern("M/d/yyyy h:mma")
  private val responseFields = List("Symbol", "StockExchange", "Ask",
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
        throw new DatabaseException("Yahoo Finance query failed.")
    }

    implicit val formats = DefaultFormats
    val root = try {
      JsonParser.parse(response)
    } catch {
      case ex: JsonParser.ParseException =>
        throw new DatabaseException("Yahoo Finance returned invalid JSON.")
    }

    val quoteMap = try {
      val count = (root\"query"\"count").extract[Int] 

      // When the response contains only one quote the quote array is omitted.
      val quoteElements = if (count == 1)
          List(root\"query"\"results"\"quote")
        else
          (root\"query"\"results"\"quote").children

      (quoteElements map { (quoteElement) => (
        Stock((quoteElement\"Symbol").extract[String]),
        Quote(
          stock      = Stock((quoteElement\"Symbol").extract[String]),
          company    = (quoteElement\"Name").extract[String],
          exchange   = (quoteElement\"StockExchange").extract[String],
          price      = BigDecimal((quoteElement\"Ask").extract[String]),
          updateTime = dateTimeFormat.parseDateTime(
              (quoteElement\"LastTradeDate").extract[String]
            + " "
            + (quoteElement\"LastTradeTime").extract[String]
          ),
          info = QuoteInfo(
            percentChange = BigDecimal((quoteElement\"ChangeinPercent")
                                        .extract[String].stripSuffix("%")),
            dividendShare = BigDecimal((quoteElement\"DividendShare").extract[String]),
            openPrice     = BigDecimal((quoteElement\"Open").extract[String]),
            lowPrice      = BigDecimal((quoteElement\"DaysLow").extract[String]),
            highPrice     = BigDecimal((quoteElement\"DaysHigh").extract[String])
          )
        )
      )}).toMap
    } catch {
      //case ex: MappingException =>
      //  throw new DatabaseException("Yahoo Finance returned JSON with unexpected structure.")

      case ex: NumberFormatException =>
        throw new DatabaseException("Yahoo Finance returned an invalid stock price.")
    }

    stocks map { (stock) => {
      (quoteMap get (stock)) match {
        case Some(quote: Quote) => quote
        case None => throw new NoSuchStockException(stock)
    }}}
  }

  // Restrict exchange and symbol to be alphabetic.
  private def buildYQL(symbols: Iterable[Stock]) = (
    "SELECT " + responseFields.mkString(",") + " FROM yahoo.finance.quotes "
      + "WHERE symbol in (" + (symbols map { "'%s'" format _.symbol }).mkString(",") + ")"
  )
}


