package stockdata

import scala.math.BigDecimal
import org.joda.time.DateTime

object TH {
  val emptyQuoteInfo = QuoteInfo(None, None, None, None, None)

  val msft_stock = Stock("MSFT")
  val msft_name  = "Microsoft Corperation"

  val appl_stock = Stock("APPL")
  val appl_name =  "Apple"

  def q1(stock: Stock = msft_stock, exchange: String = "NasdaqNM",
        company: String = msft_name,
        time: DateTime = new DateTime(),
        price: BigDecimal = BigDecimal(1.0),
        qi: QuoteInfo = emptyQuoteInfo) = {
      Quote(stock, exchange, company, price, time, qi)
  }

  def q2(stock: Stock = appl_stock, exchange: String = "NasdaqNM",
        company: String = appl_name,
        time: DateTime = new DateTime(),
        price: BigDecimal = BigDecimal(1.0),
        qi: QuoteInfo = emptyQuoteInfo) = {
      Quote(stock, exchange, company, price, time, qi)
  }

}
