
// Written by: Michael Koval

package stockdata

import java.io.IOException

import java.net.{HttpURLConnection,URL,URLDecoder,URLEncoder}
import scala.collection.immutable.ListMap
import scala.collection.mutable.{Map => MMap}
import scala.io.Source
import scala.math.BigDecimal

// Sorry, Mike, you can take this out later.
import net.liftweb.common.Loggable

class HttpQueryService(method: String) extends QueryService with Loggable {
  def query(url: URL): String = {
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod(method)

    val responseCode = connection.getResponseCode()
    if (responseCode != HttpURLConnection.HTTP_OK) {
        logger.error("Querying failed");
        logger.error(Source fromInputStream (connection.getInputStream) mkString)
        throw new IOException("Request returned response code %s.".format(responseCode))
    }

    val responseStream = connection.getInputStream()
    val responseString = Source.fromInputStream(responseStream).mkString
    connection.disconnect()
    responseString
  }
}

object HttpQueryService {
  def buildQuery(params: Iterable[(String, String)], encoding: String): String =
    (params map {
      case (key, value) =>
        (URLEncoder.encode(key, encoding) + "=" + URLEncoder.encode(value, encoding))
    }).mkString("&")

  def buildQuery(params: Iterable[(String, String)]) : String = buildQuery(params, "UTF-8")

  def parseQuery(query: String, encoding: String): ListMap[String, String] =
    ListMap((query.split("&") map (_.split("=") match {
        case Array(encodedKey, encodedValue) =>
          (URLDecoder.decode(encodedKey, encoding), URLDecoder.decode(encodedValue, encoding))
    })): _*)
}

