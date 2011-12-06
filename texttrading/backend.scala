
package texttrading

import formats._
import scalaz.Scalaz._

import model._
import model.schema._

class PitFailBackend extends Backend {

    def perform(request: Request) = {
        val Request(username, action) = request
        
        // Sorry this is a little less clean than it used to be.
        val (user, greeting) = User isNew username match {
            case NewUser(user) => (user, welcomeGreeting(username))
            case OldUser(user) => (user, Seq[String]())
        }
            
        val status : Status = action match {
            case Buy(asset)      => WithUser(user).buy(asset)
            case Sell(asset)     => WithUser(user).sell(asset)
            case SellAll(ticker) => WithUser(user).sellAll(ticker)
            case Portfolio       => WithUser(user).portfolio
            case GetInfo(a)      => WithUser(user).stockInfo(a)
        }

        Response(status, greeting)
    }

    def welcomeGreeting(name: String) = Seq(
        """|Welcome to PitFail, a made-up money trader.
           |Try it out: %s
           |"""
        .stripMargin
        .format(commandIntro)
    )
}

case class WithUser(user: User) {

    def failed: PartialFunction[Any,Failed] = {
        case e => Failed(e.toString())
    }

    // TODO: buy & sell look very similar, condense.
    def buy(asset: StockAsset) =
        try (
               asset
            |> {
                case StockShares(ticker, shares) =>
                    (ticker, user.lastPortfolio.userBuyStock(ticker, shares))
                    
                case StockDollars(ticker, dollars) =>
                    (ticker, user.lastPortfolio.userBuyStock(ticker, dollars))
            }
            |> {
                case (ticker, StockPurchase(shares, dollars, _)) =>
                    TransactionResponse(ticker, dollars, shares)
            }
        )
        catch failed

    def sell(asset: StockAsset) =
        try (
               asset
            |> {
                case StockShares(ticker, shares) =>
                    user.lastPortfolio.userSellStock(ticker, shares)
                    
                case StockDollars(ticker, dollars) => 
                    user.lastPortfolio.userSellStock(ticker, dollars)
            }
            |> { _ => OK }
        )
        catch failed

    def portfolio = StringResponse(
            user.lastPortfolio.cash.$ + " in cash," +
            (( user.lastPortfolio.myStockAssets map (_.ticker) ) mkString (", "))
        )

    def stockInfo(ticker: String) = StringResponse {
        val shares = user.lastPortfolio howManyShares ticker
        if (shares == 0) "You don't have any " + ticker
        else "You have " + shares.###() + " shares of " + ticker
    }

    def sellAll(ticker: String) =
        try {
            user.lastPortfolio.userSellAll(ticker)
            OK
        }
        catch failed
}

