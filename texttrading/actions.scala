
package texttrading

import model.{Dollars, Shares}

// Message in from the frontend:

case class Message(
    username: String,
    command:  String,
    reply:    Reply
)

trait Reply {
    def reply(text: String): Unit
    def apply(text: String) = reply(text)
}

// Format of the request:

case class Request(username: String, action: Action)

// Actions that can be performed by the user:

sealed abstract class Action
case object Portfolio extends Action
case class GetInfo(ticker: String) extends Action
case class Buy(asset: StockAsset) extends Action
case class Sell(asset: StockAsset) extends Action {
    override def toString = "Sold " + asset + "."
}

case class SellAll(ticker: String) extends Action {
    override def toString = "Sold all of " + ticker + "."
}

sealed abstract class StockAsset
case class StockDollars(ticker: String, dollars: Dollars) extends StockAsset {
    override def toString = dollars.$ + " of " + ticker
}
case class StockShares(ticker: String, shares: Shares) extends StockAsset {
    override def toString = shares + " of " + ticker
}

// Responses that can be sent back:
case class Response(status: Status, extraMsgs: Seq[String])

sealed abstract class Status
case object OK extends Status
case class TransactionResponse(ticker: String, dollars: Dollars, shares: Shares) extends Status
case class StringResponse(msg: String) extends Status
case class Failed(msg: String) extends Status

