
package stockdata

import java.io.IOException
import java.net.URL

import org.joda.time.{DateTime,Duration}
import org.joda.time.format.DateTimeFormat
import net.liftweb.common.Loggable

import model.{Dollars, Shares, Price}

case class Dividend(ticker: String, date: DateTime, price: Price)

trait DividendDatabase {
    def recentDividends(stock: Stock): Seq[Dividend]
}

class YahooDividendDatabase(queryService: QueryService)
    extends DividendDatabase
    with Loggable
{
    
    def recentDividends(stock: Stock): Seq[Dividend] = {
        val end = new DateTime
        val start = end minusDays 7
        
        val query = HttpQueryService buildQuery (Seq(
            "s" -> stock.symbol,
            "a" -> ("%d" format (start.monthOfYear.get-1)),
            "b" -> ("%d" format start.dayOfMonth.get),
            "c" -> ("%d" format start.year.get),
            "d" -> ("%d" format (end.monthOfYear.get-1)),
            "e" -> ("%d" format end.dayOfMonth.get),
            "f" -> ("%d" format end.year.get),
            "g" -> "v",
            "ignore" -> ".csv" // maybe not need this
        ), "UTF-8")
        val url = "http://ichart.finance.yahoo.com/table.csv?" + query
        logger.info("Querying " + url)
        
        val response =
            try
                queryService query (new URL(url))
            catch {
                case e: IOException =>
                    logger.error("Failing URL was " + url)
                    throw DatabaseException("Dividend query failed", e)
            }
            
        val formatSpec = "MM/dd"
        val format = DateTimeFormat forPattern formatSpec
        
        response.split("$").tail.init map { line =>
            (line.split("\\,") toList) match {
                case dateString :: priceString :: _ => Dividend(
                    stock.symbol,
                    format parseDateTime dateString,
                    Price(priceString)
                )
                
                case _ => throw DatabaseException("I do not understand " + line, null)
            }
        }
    }
}

class CachedDividendDatabase(db: DividendDatabase) extends DividendDatabase {
    type Result = Seq[Dividend]
    val map = new CacheMap[Stock,Result](new Duration(1000 * 60 * 5))
    
    def recentDividends(stock: Stock) = map.get(stock) {
        db.recentDividends(stock)
    }
}

