package com.github.pitfail

import scala.io.Source
import java.net.{HttpURLConnection,URL}

class HttpQueryService extends QueryService {
  def query(url: URL): String = {
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    val responseStream = connection.getInputStream()
    Source.fromInputStream(responseStream).mkString
  }
}

trait QueryService {
  def query(url: URL): String
}
