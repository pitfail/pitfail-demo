
package code
package model

import scala.math.{BigDecimal}

object Stocks {
    
    case class StockShares(val ticker: String, val shares: BigDecimal) {
        assert(shares > 0)
        
        def price: BigDecimal = stockPrice(ticker)
        def value: BigDecimal = price * shares
    }
    
    // This is obviously wrong
    def stockPrice(ticker: String): BigDecimal =
        BigDecimal("2.93")
    
}

