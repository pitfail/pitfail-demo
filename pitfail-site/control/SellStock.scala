
package code
package control

import net.liftweb.{common}
import common.{Loggable}

import model.Schema._
import model.Stocks.{StockShares}
import org.squeryl.PrimitiveTypeMode.inTransaction

object SellStock extends Loggable {
    
    case class DontOwnStock(ticker: String)
        extends RuntimeException
    
    def userSellAllStock(
        ticker: String
    ) {
        LoginManager withUser (u => userSellAllStock(u, ticker))
    }
    
    def userSellAllStock(
        user: User,
        ticker: String
    ) { inTransaction
    {
        val port: Portfolio = user.mainPortfolio
        port haveTicker ticker match {
            case Some(_) => ()
            case None =>
                throw DontOwnStock(ticker)
        }
        
        port.sellAll(ticker)
    }}
}


