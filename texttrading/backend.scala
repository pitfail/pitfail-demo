
package texttrading

import formats._
import scalaz.Scalaz._

import model._
import model.schema._

class PitFailBackend extends Backend {

    def perform(request: Request) = {
        val Request(username, action) = request
        
        // Sorry this is a little less clean than it used to be.
        val (user, greeting) = editDB {
            (
                User byName username,
                Seq[String]()
            )
            .orCreate {
                User ensure username map { (_, welcomeGreeting(username)) }
            }
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
                case StockShares(ticker, shares) => editDB {
                    user.mainPortfolio.buyStock(ticker, shares)
                }
                    
                case StockDollars(ticker, dollars) => editDB {
                    user.mainPortfolio.buyStock(ticker, dollars)
                }
            }
            |> {
                case StockPurchase(_, shares, dollars) =>
                    TransactionResponse(asset, dollars, shares)
            }
        )
        catch failed

    def sell(asset: StockAsset) =
        try (
               asset
            |> {
                case StockShares(ticker, shares) => editDB {
                    user.mainPortfolio.sellStock(ticker, shares)
                }
                case StockDollars(ticker, dollars) => editDB {
                    user.mainPortfolio.sellStock(ticker, dollars)
                }
            }
            |> { _ => OK }
        )
        catch failed

    def portfolio = StringResponse(
            user.mainPortfolio.cash.$ + " in cash," +
            (( user.mainPortfolio.myStockAssets map (_.ticker) ) mkString (", "))
        )

    def stockInfo(ticker: String) = StringResponse(
        user.mainPortfolio.haveTicker(ticker) match {
            case Some(a) => "Have " + a
            case None    => "You don't have any " + ticker + "."
        }
    )


    def sellAll(ticker: String) =
        try {
            user.mainPortfolio.sellAll(ticker)
            OK
        }
        catch failed
}

