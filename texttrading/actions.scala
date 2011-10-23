
package texttrading

// Format of the request:

case class Request(username: String, action: Action)

// Actions that can be performed by the user:

sealed abstract class Action
case class Buy(asset: StockAsset) extends Action
case class Sell(asset: StockAsset) extends Action
case class SellAll(ticker: String) extends Action

sealed abstract class StockAsset
case class StockShares(ticker: String, volume: BigDecimal) extends StockAsset
case class StockVolume(ticker: String, volume: BigDecimal) extends StockAsset

// Responses that can be sent back:

case class Response(status: Status, extraMsgs: Seq[String])

sealed abstract class Status
case object OK extends Status
case class Failed(msg: String) extends Status

