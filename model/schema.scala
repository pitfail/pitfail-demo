
package model

import errors._
import org.joda.time.DateTime

/*

Welcome to the Schema
=====================

The various parts of the schema are in the following files:
 - users.scala        -- basic aspects of users + portfolios
 - stocks.scala       -- stocks
 - derivatives.scala  -- derivatives
 - auctions.scala     -- auctions
 - news.scala         -- news events (for the sidebar)

Because of implicits, you should

    import model.schema._

before using the database.
 
Reading the database
--------------------

You must read the database inside a readDB block:

    readDB {
        val events = recentEvents(10)
        val items = events map (ev => <li>{ev}</li>)
        
        <ul>{items}</ul>
    }

Editing the database
--------------------
 
You must edit the database inside a editDB block:

    editDB {
        User ensure "joe" // Make user the "joe" account exists
    }

Database operations are monadic! This means it is often necessary to use a for
block:

    editDB {
        for {
            user <- User ensure "joe"
            _ <- user.mainPortfolio.buyStock("MSFT", Dollars(500))
        }
        yield ()
    }

*/

object schema
    extends DummySchema
    with DBMagic  // Updates, selects, etc
    with SchemaErrors
    with UserSchema
    with StockSchema
    with DerivativeSchema
    with AuctionSchema
    with NewsSchema
    
trait SchemaErrors {
    case object NegativeVolume extends BadUser
    case class NotEnoughCash(have: Dollars, need: Dollars) extends BadUser
    case class DontOwnStock(ticker: String) extends BadUser
    case class NotEnoughShares(have: Shares, need: Shares) extends BadUser
    case object OfferExpired extends BadUser
    case object NotExecutable extends BadUser
    case object NoSuchAuction extends BadUser
    case class BidTooSmall(going: Dollars) extends BadUser
    case object NoSuchUser extends BadUser
    case object NoSuchDerivativeAsset extends BadUser
}

case object NotFound extends BadUser

