
package model

trait UserSchema {
    self: StockSchema
        with DerivativeSchema
        with AuctionSchema
        with CommentSchema
        with DBMagic 
        with SchemaErrors =>
    
    val startingCash = Dollars("200000")
            
    implicit val users = table[User]
    implicit val portfolios = table[Portfolio]
    
    // Model tables
    
    case class User(
            id:               Key,
            username:         String,
            mainPortfolio:    Link[Portfolio],
            votingPortfolio:  Link[Portfolio]
        )
        extends KL
        with UserWithComments

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
        
    // Detailed Operations
        
    object User {
        // If this user doesn't already exist, create it
        private[model] def ensure(name: String): Transaction[User] =
            byName(name).orCreate(newUser(name))
        
        private[model] def ensureP(name: String): Transaction[Portfolio] = {
            def port: Portfolio = byName(name).mainPortfolio
            port orCreate newUserP(name)
        }
        
        private[model] def byName(name: String) = {
            val u = (users filter (_.username == name)).headOption
            u getOrElse (throw NoSuchUser)
        }
        
        // Create a new user
        private[model] def newUserAll(name: String) = mutually { (u, p1, p2) =>
            for {
                user    <- User(id=u, name=name, mainPortfolio=p1, votingPortfolio=p2).insert
                mainP   <- Portfolio(id=p1, owner=u, cash=startingCash)
                votingP <- Portfolio(id=p2, owner=u, cash=startingCash)
            }
            yield (user, mainP, votingP)
        }
        
        private[model] def newUser(name: String) = newUserAll(name) map (_._1)
        private[model] def newUserP(name: String) = newUserAll(name) map (_._2)
        
    }
}

