package stockdata

import java.net.URL

trait QueryService {
  def query(url: URL): String
}
