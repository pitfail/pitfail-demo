
package texttrading

import formats._
import model.Schema._
import scalaz.Scalaz._

class PitFailBackend extends Backend {
    
    def perform(request: Request) = {
        val Request(username, action) = request
        val (user, greeting) = byUsername(username) match {
            case Some(user) => (user, Nil)
            case None       => (ensureUser(username), welcomeGreeting(username))
        }
        
        val status = action match {
            case Buy(asset)      => WithUser(user).buy(asset)
            case Sell(asset)     => WithUser(user).sell(asset)
            case SellAll(ticker) => WithUser(user).sellAll(ticker)
        }
        
        Response(status, greeting)
    }
    
    def welcomeGreeting(name: String) = Seq(
        """| Welcome to PitFail!
           |
           | PitFail lets you experiment trading stocks with made-up money.
           |
           | Try out these commands:
           | %s
           |"""
        .stripMargin
        .format(commandIntro)
    )
}

case class WithUser(user: User) {
    
    def failed: PartialFunction[Any,Failed] = {
        case e => Failed(standardMessage(e))
    }
    
    def buy(asset: StockAsset) =
        try (
               asset
            |> toVolume _
            |> { case StockVolume(ticker, volume) =>
                    user.mainPortfolio.buyStock(ticker, Dollars(volume))
               }
            |> { _ => OK }
        )
        catch failed
        
    def sell(asset: StockAsset) =
        try (
               asset
            |> toVolume _
            |> { case StockVolume(ticker, volume) =>
                    user.mainPortfolio.sellStock(ticker, Dollars(volume))
               }
            |> { _ => OK }
        )
        catch failed
        
    def sellAll(ticker: String) =
        try {
            user.mainPortfolio.sellAll(ticker)
            OK
        }
        catch failed
        
    def toVolume(asset: StockAsset) = asset match {
        case v @ StockVolume(_,_)       => v
        case StockShares(ticker,shares) =>
            // TODO: You know...
            StockVolume(ticker, shares / 3.14)
    }
}

