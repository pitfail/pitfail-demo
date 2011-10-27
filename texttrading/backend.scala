
package texttrading

import formats._
import model._
import model.Schema._
import scalaz.Scalaz._

class PitFailBackend extends Backend {

    def perform(request: Request) = {
        val Request(username, action) = request
        val (user, greeting) = byUsername(username) match {
            case Some(user) => (user, Nil)
            case None       => (ensureUser(username), welcomeGreeting(username))
        }

        val status : Status = action match {
            case Buy(asset)      => WithUser(user).buy(asset)
            case Sell(asset)     => WithUser(user).sell(asset)
            case SellAll(ticker) => WithUser(user).sellAll(ticker)
            case Portfolio       => WithUser(user).portfolio
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
        case e => Failed(standardMessage(e))
    }

    /* TODO: buy & sell look very similar, condense. */
    def buy(asset: StockAsset) =
        try (
               asset
            |> { case StockShares(ticker, shares) =>
                    user.mainPortfolio.buyStock(ticker, shares)
                 case StockDollars(ticker, dollars) =>
                    user.mainPortfolio.buyStock(ticker, dollars)
            }
            |> TransactionResponse
        )
        catch failed

    def sell(asset: StockAsset) =
        try (
               asset
            |> {
               case StockShares(ticker, shares) =>
                    user.mainPortfolio.sellStock(ticker, shares)
               case StockDollars(ticker, dollars) =>
                    user.mainPortfolio.sellStock(ticker, dollars)
               }
            |> { _ => OK }
        )
        catch failed

    def portfolio = StringResponse("This should be your portfolio, sorry.")

    def sellAll(ticker: String) =
        try {
            user.mainPortfolio.sellAll(ticker)
            OK
        }
        catch failed
}

