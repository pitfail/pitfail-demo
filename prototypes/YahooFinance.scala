import java.io.{BufferedReader,InputStreamReader}
import java.net.{HttpURLConnection,URL,URLEncoder}
import scala.io.{Source}

def buildURL(baseUrl: String,
             params: Iterable[(String, String)],
             encoding: String): URL = {
    val queryString = params map {
        case (key, value) =>
            (URLEncoder.encode(key, encoding) + "=" + URLEncoder.encode(value, encoding))
    }
    return new URL(baseUrl + "?" + queryString.mkString("&"))
}

def parseQueryStocksResponse(response: String): List[Double] = {
    println(response)
    return null
}

def queryStocks(symbols: List[String]) = {
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

    parseQueryStocksResponse(responseString)
}

queryStocks(List("MSFT", "AAPL"))
