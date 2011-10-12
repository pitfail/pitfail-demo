
package code
package control

import net.liftweb.{common}
import common.{Loggable}

import model.Schema._
import model.Stocks.{StockShares}
import org.squeryl.PrimitiveTypeMode.inTransaction

object BuyStock extends Loggable {
    
    case class NotEnoughCash(
            val have: BigDecimal,
            val needed: BigDecimal
        ) extends RuntimeException
    
    def userBuyStock(
        ticker: String,
        volume: BigDecimal
    ) {
        LoginManager withUser {u => userBuyStock(u, ticker, volume)}
    }
    
    def userBuyStock(
        user: User,
        ticker: String,
        volume: BigDecimal
    ) { inTransaction {
        val port: Portfolio = user.mainPortfolio
        // TODO: This is obviously wrong
        val shares = StockShares(ticker, BigDecimal("25.00"))
        val cashOnHand = port.cash
        val cashNeeded = shares.price
        
        if (cashOnHand < cashNeeded)
            throw NotEnoughCash(cashOnHand, cashNeeded)
        
        port.buy(shares)
    }}
}

