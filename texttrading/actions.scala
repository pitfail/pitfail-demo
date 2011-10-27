
package texttrading

import model.{Dollars, Shares, Schema}

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
case class Buy(asset: StockAsset) extends Action
case class Sell(asset: StockAsset) extends Action
case class SellAll(ticker: String) extends Action

sealed abstract class StockAsset
case class StockDollars(ticker: String, dollars: Dollars) extends StockAsset
case class StockShares(ticker: String, shares: Shares) extends StockAsset

// Responses that can be sent back:
case class Response(status: Status, extraMsgs: Seq[String])

sealed abstract class Status
case object OK extends Status
case class TransactionResponse(sa :Schema.StockAsset) extends Status
case class StringResponse(msg: String) extends Status
case class Failed(msg: String) extends Status

