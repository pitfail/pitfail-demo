
package model

object operations
{
    import schema._
    
    // For Java interop, we declare a lot of high level operations here.
    // Of coures, there's no reason not to use these in the rest of the code too.
    
    def userBuyStock(username: String, ticker: String, dollars: Dollars) = editDB {
        val user = User userEnsure username
        for {
            _ <- user.lastPortfolio.buyStock(ticker, dollars)
        }
        yield ()
    }
    
    def userSellStock(username: String, ticker: String) = editDB {
        val user = User userEnsure username
        for {
            _ <- user.lastPortfolio.sellAll(ticker)
        }
        yield ()
    }
    
    def getUserPortfolio(username: String): Portfolio = {
        val user = User userEnsure username
        user.lastPortfolio
    }
    
    def getUser(username: String): User = {
        User userEnsure username
    }
    
    def getDefaultLeague(): League = readDB {
        League.default
    }
}


