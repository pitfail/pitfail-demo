
package model

trait UserSchema {
    self: StockSchema
        with DerivativeSchema
        with AuctionSchema
        with DBMagic 
        with SchemaErrors =>
    
    val startingCash = Dollars("200000")
            
    implicit val users = table[User]
    implicit val portfolios = table[Portfolio]
    
    // Model tables
    
    case class User(
            id:            Key,
            username:      String,
            mainPortfolio: Link[Portfolio]
        )
        extends KL

    case class Portfolio(
            id:    Key,
            cash:  Dollars,
            owner: Link[User],
            loan:  Dollars
        )
        extends KL
        with PortfolioWithStocks
        with PortfolioWithDerivatives
        with PortfolioWithAuctions
    
    // Operations
        
    object User {
        // If this user doesn't already exist, create it
        def ensure(name: String): Transaction[User] =
            byName(name).orCreate(newUser(name))
        
        def byName(name: String) = {
            val u = (users filter (_.username == name)).headOption
            u getOrElse (throw NoSuchUser)
        }
        
        // Create a new user
        def newUser(name: String) = mutually { (u, p) =>
            for {
                user <- User(u, name, p).insert
                _    <- Portfolio(p, startingCash, u, Dollars("0")).insert
            }
            yield user
        }
    }
}

