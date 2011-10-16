package com.github.pitfail

import java.net.URL

trait QueryService {
  def query(url: URL): String
}
