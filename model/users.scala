
package model

import org.joda.time.DateTime
import spser._
import scala.collection.JavaConversions._

trait UserSchema extends Schema {
    self: StockSchema with DerivativeSchema with AuctionSchema with CommentSchema
        with DBMagic with SchemaErrors with VotingSchema with AutoTradeSchema with DividendSchema =>

    implicit val uCon = User.apply _
    implicit val pCon = Portfolio.apply _
    implicit val oCon = Ownership.apply _
    implicit val piCon = PortfolioInvite.apply _
    implicit val pvCon = PortfolioValue.apply _
    implicit val lCon = League.apply _
    implicit val aCon = Administration.apply _
    implicit val subCon = Subscription.apply _
            
    implicit val users: Table[User] = table[User]
    implicit val portfolios: Table[Portfolio] = table[Portfolio]
    implicit val ownerships: Table[Ownership] = table[Ownership]
    implicit val portfolioInvites: Table[PortfolioInvite] = table[PortfolioInvite]
    implicit val portfolioValues: Table[PortfolioValue] = table[PortfolioValue]
    implicit val leagues: Table[League] = table[League]
    implicit val administrations: Table[Administration] = table[Administration]
    implicit val subscriptions: Table[Subscription] = table[Subscription]
    
    abstract override def tables = (
           users :: portfolios :: ownerships :: portfolioInvites
        :: portfolioValues :: leagues :: administrations :: subscriptions :: super.tables )
    
    // Model tables

    case class User(
            id:            Key = nextID,
            username:      String,
            lastPortfolio: Link[Portfolio]
        )
        extends KL
        with UserOps
        with UserWithComments
        with UserWithLeagues

    case class Portfolio(
            id:     Key = nextID,
            league: Link[League],
            name:   String,
            cash:   Dollars,
            loan:   Dollars,
            rank:   Int
        )
        extends KL
        with PortfolioOps
        with PortfolioWithStocks
        with PortfolioWithDerivatives
        with PortfolioWithAuctions
        with PortfolioWithVotes
        with PortfolioWithAutoTrades
        with PortfolioWithDividends

    case class PortfolioValue(
            id:        Key = nextID,
            dateTime:  DateTime,
            portfolio: Link[Portfolio],
            dollars:   Dollars
        )
        extends KL
        
    case class Ownership(
            id:        Key = nextID,
            user:      Link[User],
            portfolio: Link[Portfolio]
        )
        extends KL
        
    case class PortfolioInvite(
            id:   Key = nextID,
            from: Link[Portfolio],
            to:   Link[User]
        )
        extends KL

    case class League(
            id:           Key = nextID,
            name:         String,
            startingCash: Dollars,
            owner:        Link[User]
        )
        extends KL
        with LeagueOps
        
    // Ownership of a league
    case class Administration(
            id:     Key = nextID,
            user:   Link[User],
            league: Link[League]
        )
        extends KL

    case class Subscription(
            id:    Key = nextID,
            user:  Link[User],
            email: String
        )
        extends KL
        
    // Detailed Operations

    trait UserWithLeagues {
        self: User =>

        def newLeague(name: String, cash: Dollars) : League = {
            if (cash <= Dollars(0))
                throw NonPositiveDollars

            editDB {
                if (League byName name isDefined) {
                    throw NameInUse
                }
                League(name=name, startingCash=cash, owner=self) insert
            }
        }
    }


    trait UserOps {
        self: User =>
        
        def myPortfolios: List[Portfolio] = readDB {
            (ownerships where ('user ~=~ this)).toList map (_.portfolio.extract) toList
        }
        
        def aPortfolio = myPortfolios head
        
        // Java inter-op
        def getPortfolios: java.util.List[Portfolio] = myPortfolios
        
        def getCurrentPortfolio: Portfolio = lastPortfolio
        
        def userCreatePortfolio(name: String): Portfolio = editDB(createPortfolio(name))
        
        def portfolioByName(name: String) = readDB (
            (myPortfolios filter (_.name == name)).headOption getOrElse (throw NoSuchPortfolio)
        )
        
        def userSwitchPortfolio(port: Portfolio) = editDB(switchPortfolio(port))
        
        def myPortfolioInvites: List[PortfolioInvite] = readDB {
            portfolioInvites where ('to ~=~ this) toList
        }
        
        def userAcceptInvite(invite: PortfolioInvite) = editDB(acceptInvite(invite))
        
        def userDeclineInvite(invite: PortfolioInvite) = editDB(declineInvite(invite))
        
        def userCreateLeague(name: String, startingCash: Dollars) =
            editDB(createLeague(name, startingCash))
        
        def leaguesIAdminister: List[League] = ((administrations where ('user ~=~ this)).toList
            map (_.league.extract))
        
        def doIAdminister(league: League) = !(administrations where ('user ~=~ this)
            where ('league ~=~ league)).headOption.isEmpty
        
        def userSubscribeToNewsletter(email: String) = editDB {
            Subscription(user=this, email=email).insert
        }
        
        private[model] def createPortfolio(name: String) = {
            if (!(portfolios where ('name ~=~ name)).headOption.isEmpty) throw NameInUse

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
            
        private[model] def createLeague(name: String, startingCash: Dollars) = {
            League byName name match {
                case Some(_) => throw NameInUse
                case None =>
            }
            for {
                league <- League(name=name, startingCash=startingCash, owner=self).insert
                _ <- Administration(user=this, league=league).insert
            }
            yield league
        }
    }
    
    object User {
        def byName(name: String) = readDB {
            val u = (users where ('username ~=~ name)).headOption
            u getOrElse (throw NoSuchUser)
        }
        
        def userEnsure(name: String): User = readDB {
            try {
                byName(name)
            }
            catch {
                case NoSuchUser =>
                    val league = League.default
                    val cash = league.startingCash
                    editDB {
                        for {
                            port   <- Portfolio(name=name, league=league,
                                cash=cash, loan=cash, rank=0).insert
                            user   <- User(username=name, lastPortfolio=port).insert
                            _      <- Ownership(user=user, portfolio=port).insert
                        }
                        yield user
                    }
            }
        }
        
        def isNew(name: String) = {
            try OldUser(byName(name))
            catch { case NoSuchUser => NewUser(userEnsure(name)) }
        }
    }

    object Portfolio {
        def byID(id: Key) =
            portfolios lookup id getOrElse (throw NoSuchPortfolio)
        
        def byName(name: String) : Portfolio =
            (portfolios where ('name ~=~ name)).headOption getOrElse {
                throw NoSuchPortfolio
            }

        def byLeague(league: League) = portfolios where ('league ~=~ league) toList
    }

    object PortfolioValues {
        def history(portfolio: Portfolio, begin: DateTime, end: DateTime) = readDB {
            import org.scala_tools.time.Imports._

            portfolioValues.toList filter { pv =>
                portfolio~~pv.portfolio && begin <= pv.dateTime && pv.dateTime <= end
            } map { pv =>
                (pv.dateTime, pv.dollars)
            }
        }
    }
    
    sealed trait IsNewUser
    case class NewUser(user: User)
    case class OldUser(user: User)

    trait PortfolioOps {
        self: Portfolio with PortfolioWithStocks with PortfolioWithDerivatives =>

        // Java inter-op
        def getLeague: League = readDB { league }
            
        def spotValue: Dollars = readDB (
              cash
            + (myStockAssets map (_.dollars)).foldLeft(Dollars(0))(_+_)
            + (myDerivativeAssets map (_.spotValue)).foldLeft(Dollars(0))(_+_)
        )

        def userInviteUser(user: User) = editDB(inviteUser(user))
        def userInviteUser(name: String) = editDB(inviteUser(name))

        def owners: List[User] = readDB {
            (ownerships where ('portfolio ~=~ this)).toList map (_.user.extract)
        }

        def isOwnedBy(user: User): Boolean = owners exists (_ ~~ user)

        private[model] def inviteUser(user: User) =
            PortfolioInvite(from=this, to=user).insert
        
        private[model] def inviteUser(name: String) =
            PortfolioInvite(from=this, to=(User byName name)).insert
    }

    object League {
        val defaultName = "default"
        val defaultStartingCash = Dollars(200000)
        val defaultOwnerName = "nobody"

        def default = readDB {
            byName(defaultName) match {
                case None => editDB {
                    for {
                        user   <- User(username=defaultOwnerName, lastPortfolio="").insert
                        league <- League(name=defaultName, startingCash=defaultStartingCash, owner=user).insert
                    }
                    yield league
                }
                case Some(league) => league
            }
        }
        
        def byName(name: String) = leagues where ('name ~=~ name) headOption
        def byID(id: Key) = leagues where ('id ~=~ id) headOption
    }
    
    trait LeagueOps {
        self: League =>
        
        // Java inter-op
        def getLeaders(n: Int): java.util.List[Portfolio] = readDB {
            (portfolios where ('league ~=~ this)).toList sortBy (_.rank) take n
        }
    }
    
}

