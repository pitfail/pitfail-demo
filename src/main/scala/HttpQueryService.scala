package com.github.pitfail

import java.io.IOException

import java.net.{HttpURLConnection,URL,URLDecoder,URLEncoder}
import scala.collection.immutable.ListMap
import scala.collection.mutable.{Map => MMap}
import scala.io.Source
import scala.math.BigDecimal

class HttpQueryService(method: String) extends QueryService {
  def query(url: URL): String = {
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod(method)

    val responseCode = connection.getResponseCode()
    if (responseCode != HttpURLConnection.HTTP_OK) {
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

  def parseQuery(query: String, encoding: String): ListMap[String, String] =
    ListMap((query.split("&") map (_.split("=") match {
        case Array(encodedKey, encodedValue) =>
          (URLDecoder.decode(encodedKey, encoding), URLDecoder.decode(encodedValue, encoding))
    })): _*)
}
