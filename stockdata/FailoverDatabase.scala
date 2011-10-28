
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

class FailoverStockDatabase(databases: List[StockDatabase]) extends StockDatabase with Loggable {
  case class ResponseOption()
  case class ResponseValid(quotes: Iterable[Quote])
  case class ResponseError(error: Throwable)

  def getQuotes(stocks: Iterable[Stock]): Iterable[Quote] = {
    var error: Throwable = new DatabaseException("No stock databases were specified", null)
    
    for (database <- databases) {
      tryQueryDatabase(database, stocks) match {
        case ResponseValid(quotes) =>
          return quotes

        case ResponseError(ex) => {
          logger.error("Database Query failed with error.")
          error = ex
        }
      }
    }
    logger.error("All database queries failed.")
    throw error
  }

  def tryQueryDatabase(database: StockDatabase, stocks: Iterable[Stock]) =
    try {
      ResponseValid(database.getQuotes(stocks))
    } catch {
      case ex: Throwable =>
        ResponseError(ex)
    }
}
