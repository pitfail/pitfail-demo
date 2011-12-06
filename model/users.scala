
package model

trait UserSchema {
    self: StockSchema
        with DerivativeSchema with AuctionSchema with CommentSchema
        with DBMagic with SchemaErrors with VotingSchema with AutoTradeSchema =>

    implicit val users            = table[User]
    implicit val portfolios       = table[Portfolio]
    implicit val ownerships       = table[Ownership]
    implicit val portfolioInvites = table[PortfolioInvite]
    implicit val leagues	  = table[League]
    
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
            league: Link[League],
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

    case class League(
            id: Key = nextID,
            name: String,
            startingCash: Dollars
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

            val league = League.default
            val cash = league.startingCash

            for {
                /* XXX: change to a different league with param. */
                port <- Portfolio(name=name, league=league, cash=cash, loan=cash, rank=1000).insert
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
        // XXX: this duplicates logic in createPortfolio
        private[model] def newUserAll(name: String) = {

            val league = League.default
            val cash = league.startingCash
            for {
                port  <- Portfolio(name=name, league=League.default, cash=cash, loan=cash, rank=0).insert
                user  <- User(username=name, lastPortfolio=port).insert
                _     <- Ownership(user=user, portfolio=port).insert
            }
            yield (user, port)
        }

        private[model] def newUser (name: String) : Transaction[User] =
            newUserAll(name) map (_._1)
        private[model] def newUserP(name: String) : Transaction[Portfolio] =
            newUserAll(name) map (_._2)
    }

    object Portfolio {
        def byID(id: Key) =
            portfolios lookup id getOrElse (throw NoSuchPortfolio)
        
        def byName(name: String) : Portfolio =
            (portfolios filter (_.name==name)).headOption getOrElse {
                throw NoSuchPortfolio
            }

        def byLeague(league: League) = portfolios filter (_.league.id==league.id)

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

    object League {
        val defaultName = "default"
        val defaultStartingCash = Dollars(200000)

        /* XXX: Embedding an editDB here is nasty */
        def leagueEnsure(name: String) : League = editDB {
            byName(name).orCreate(
                League(name=name,startingCash=defaultStartingCash).insert
            )
        }

        /* XXX: DB reads and writes need to be composed from the same monad
         * so that I can avoid this unconditional 'get'
         * initially the code was:
         *  def default() = League byName defaultName getOrElse (League(name=defaultName).insert)
         * which fails as   ^^^^^^^^^^^^^^^^^^^^^^^^^ is not    ^^^^^^^^^^^^^^^^^
         */
        def default() = leagueEnsure(defaultName)

        def byName(name: String) = (leagues filter (_.name==name)).headOption
        def byID(id: Key) = leagues lookup id
    }
}

