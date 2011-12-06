
package model

import org.joda.time.DateTime

trait UserSchema {
    self: StockSchema
        with DerivativeSchema with AuctionSchema with CommentSchema
        with DBMagic with SchemaErrors with VotingSchema with AutoTradeSchema =>
    
    val startingCash = Dollars(200000)
            
    implicit val users            = table[User]
    implicit val portfolios       = table[Portfolio]
    implicit val ownerships       = table[Ownership]
    implicit val portfolioInvites = table[PortfolioInvite]
    implicit val portfolioValues  = table[PortfolioValue]
    
    // Model tables
    
    case class User(
            id:            Key = nextID,
            username:      String,
            lastPortfolio: Link[Portfolio]
        )
        extends KL
        with UserOps
        with UserWithComments

    case class Portfolio(
            id:    Key = nextID,
            name:  String,
            cash:  Dollars,
            loan:  Dollars,
            rank:  Int
        )
        extends KL
        with PortfolioOps
        with PortfolioWithStocks
        with PortfolioWithDerivatives
        with PortfolioWithAuctions
        with PortfolioWithVotes
        with PortfolioWithAutoTrades

    case class PortfolioValue(
            id:        Key = nextID,
            dateTime:  DateTime,
            portfolio: Portfolio,
            dollars:   Dollars
        )
        extends KL
        
    case class Ownership(
            id:        Key = nextID,
            user:      User,
            portfolio: Portfolio
        )
        extends KL
        
    case class PortfolioInvite(
            id:   Key = nextID,
            from: Portfolio,
            to:   User
        )
        extends KL
    
    // Detailed Operations
    
    trait UserOps {
        self: User =>
        
        def myPortfolios: List[Portfolio] = readDB {
            ownerships filter (_.user ~~ this) map (_.portfolio) toList
        }
        
        def userCreatePortfolio(name: String): Portfolio = editDB(createPortfolio(name))
        
        def portfolioByName(name: String) = readDB (
            (ownerships filter (_.user ~~ this) map (_.portfolio)
                filter (_.name == name)).headOption getOrElse (throw NoSuchPortfolio)
        )
        
        def userSwitchPortfolio(port: Portfolio) = editDB(switchPortfolio(port))
        
        def myPortfolioInvites: List[PortfolioInvite] = readDB {
            portfolioInvites filter (_.to ~~ this) toList
        }
        
        def userAcceptInvite(invite: PortfolioInvite) = editDB(acceptInvite(invite))
        
        def userDeclineInvite(invite: PortfolioInvite) = editDB(declineInvite(invite))
        
        private[model] def createPortfolio(name: String) = {
            if (portfolios exists (_.name == name)) throw NameInUse
            
            for {
                port <- Portfolio(name=name, cash=startingCash, loan=startingCash, rank=1000).insert
                _    <- Ownership(user=this, portfolio=port).insert
            }
            yield (port)
        }
        
        private[model] def switchPortfolio(port: Portfolio) =
            this update (t => t copy (lastPortfolio=port))
        
        private[model] def acceptInvite(invite: PortfolioInvite) =
            for {
                _ <- Ownership(user=this, portfolio=invite.from).insert
                _ <- invite.delete
            }
            yield ()
            
        private [model] def declineInvite(invite: PortfolioInvite) =
            invite.delete
    }
    
    object User {
        def byName(name: String) = {
            val u = (users filter (_.username == name)).headOption
            u getOrElse (throw NoSuchUser)
        }
        
        def userEnsure(name: String) = editDB { ensure(name) }
        
        def isNew(name: String) = editDB {
            try Transaction(OldUser(byName(name)))
            catch { case NoSuchUser => ensure(name) map (NewUser(_)) }
        }
        
        // If this user doesn't already exist, create it
        private[model] def ensure(name: String): Transaction[User] =
            byName(name).orCreate(newUser(name))
        
        private[model] def ensureP(name: String): Transaction[Portfolio] = {
            def port: Portfolio = byName(name).lastPortfolio
            port orCreate newUserP(name)
        }
        
        // Create a new user
        private[model] def newUserAll(name: String) =
            for {
                port  <- Portfolio(name=name, cash=startingCash, loan=Dollars(0), rank=0).insert
                user  <- User(username=name, lastPortfolio=port).insert
                _     <- Ownership(user=user, portfolio=port).insert
            }
            yield (user, port)
        
        private[model] def newUser(name: String) = newUserAll(name) map (_._1)
        private[model] def newUserP(name: String) = newUserAll(name) map (_._2)
    }
    
    object Portfolio {
        def byID(id: Key) =
            portfolios lookup id getOrElse (throw NoSuchPortfolio)
        
        def byName(name: String) =
            (portfolios filter (_.name==name)).headOption getOrElse {
                throw NoSuchPortfolio
            }
    }
    
    sealed trait IsNewUser
    case class NewUser(user: User)
    case class OldUser(user: User)
    
    trait PortfolioOps {
        self: Portfolio with PortfolioWithStocks with PortfolioWithDerivatives =>
        
        def spotValue: Dollars = (
              cash
            + (myStockAssets map (_.dollars)).foldLeft(Dollars(0))(_+_)
            + (myDerivativeAssets map (_.spotValue)).foldLeft(Dollars(0))(_+_)
        )
        
        def userInviteUser(user: User) = editDB(inviteUser(user))
        
        def owners: List[User] = readDB {
            ownerships filter (_.portfolio~~this) map (_.user) toList
        }
        
        def isOwnedBy(user: User): Boolean = owners exists (_ ~~ user)
        
        private[model] def inviteUser(user: User) =
            PortfolioInvite(from=this, to=user).insert
    }
}

