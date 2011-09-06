import java.io.{BufferedReader,InputStreamReader}
import java.net.{HttpURLConnection,URL,URLEncoder}
import scala.io.{Source}
import scala.math.BigDecimal
import net.liftweb.json.JsonParser
import net.liftweb.json.JsonAST.{JInt,JValue}

object YahooFinance {
    def buildURL(baseUrl: String,
                 params: Iterable[(String, String)],
                 encoding: String): URL = {
        val queryString = params map {
            case (key, value) =>
                (URLEncoder.encode(key, encoding) + "=" + URLEncoder.encode(value, encoding))
        }
        new URL(baseUrl + "?" + queryString.mkString("&"))
    }

    def queryStocks(symbols: List[String]): List[StockQuote] = {
        implicit val formats = net.liftweb.json.DefaultFormats

        val yql = "SELECT * FROM yahoo.finance.quoteslist WHERE symbol='" + symbols.mkString(",") + "'"
        val queryString = Map(
            "q"      -> yql,
            "format" -> "json",
            "env"    -> "store://datatables.org/alltableswithkeys"
        )
        val url = buildURL("http://query.yahooapis.com/v1/public/yql", queryString, "UTF-8")

        val connection = url.openConnection().asInstanceOf[HttpURLConnection]
        val responseStream = connection.getInputStream()
        val responseString = Source.fromInputStream(responseStream).mkString

        val root = JsonParser.parse(responseString)
        val count = (root\"query"\"count").extract[Int]
        val quotes = root\"query"\"results"\"quote"

        quotes.children map((quote) => {
            val symbol = (quote\"Symbol").extract[String]
            val price  = (quote\"LastTradePriceOnly").extract[String]
            new StockQuote(symbol, BigDecimal(price))
        })
    }

    def main(args: Array[String]) {
        try {
            val quotes = queryStocks(List("MSFT", "AAPL"))
            println(quotes)
        } catch {
            case e: Exception => e.printStackTrace
        }
    }

    class StockQuote(symbol: String, price: BigDecimal) {
        var Symbol: String = symbol
        var Price: BigDecimal = price

        override def toString: String = {
            "(" + Symbol + ": " + Price + ")"
        }
    }
}
