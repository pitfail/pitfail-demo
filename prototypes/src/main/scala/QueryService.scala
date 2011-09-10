package com.github.pitfail

import java.io.IOException
import scala.io.Source
import java.net.{HttpURLConnection,URL,URLDecoder,URLEncoder}

class HttpQueryService(method: String) extends QueryService {
  def query(url: URL): String = {
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod(method)

    val responseCode = connection.getResponseCode()
    if (responseCode != HttpURLConnection.HTTP_OK) {
      throw new IOException("Request returned response code %s.".format(responseCode))
    }

    val responseStream = connection.getInputStream()
    Source.fromInputStream(responseStream).mkString
  }
}

trait QueryService {
  def query(url: URL): String
}

object HttpQueryService {
  def buildQuery(params: Map[String, String], encoding: String): String =
    (params map {
      case (key, value) =>
        (URLEncoder.encode(key, encoding) + "=" + URLEncoder.encode(value, encoding))
    }).mkString("&")

  def parseQuery(query: String): Map[String, String] =
    (query.split("&") map (_.split("=") match {
        case Array(encodedKey, encodedValue) =>
          (URLDecoder.decode(encodedKey, "ASCII"), URLDecoder.decode(encodedValue, "ASCII"))
    })).toMap
}
