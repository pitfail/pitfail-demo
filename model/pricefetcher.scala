
package model

import model.schema._
import code.control._
import scala.math.BigDecimal
import stockdata._
import org.joda.time.Duration
import scala.collection.JavaConversions._
import net.liftweb.common.Loggable

object PeriodicPortfolioEvaluator extends Loggable {
    def run(): Unit = {
        import net.liftweb.util.Helpers._

        periodically (5 minutes) {
            PeriodicPortfolioEvaluator.poll()
        }
    }

    def poll() = readDB {
        schema.portfolios.toList map {
            portfolio => editDB {
                import org.scala_tools.time.Imports._

                PortfolioValue(
                    dateTime  = DateTime.now,
                    portfolio = portfolio,
                    dollars   = portfolio.spotValue
                ).insert
            }
        }
    }
}

object StockPriceSource extends CachedStockDatabase(
    new FailoverStockDatabase(List(
      new YahooCSVStockDatabase(new HttpQueryService("GET")),
      new YahooStockDatabase(new HttpQueryService("GET"))
    )),
    // TODO: This timeout should be moved to a configuration file.
    new Duration(1000 * 60 * 5)
)

object DividendSource extends CachedDividendDatabase(
    new YahooDividendDatabase(new HttpQueryService("GET"))
)

