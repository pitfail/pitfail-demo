package pitfail.rest

import net.liftweb.http._
import net.liftweb.http.rest._

trait Convertable {
	def toXml: Elem
	def toJson: JValue
}

implicit def cvt: JxCvtPF[Convertable] = {
	case (JsonSelect, c, _) => c.toJson
	case (XmlSelect, c, _) => c.toXml
}

object Rest extends RestHelper {
	trait Convertable {
		def toXml: Elem
		def toJson: JValue
	}

	implicit def cvt: JxCvtPF[Convertable] = {
		case (JsonSelect, c, _) => c.toJson
		case (XmlSelect,  c, _) => c.toXml
	}

	def sellAsset(p: Portfolio): Box[SellInfo] = {
	}

	def buyStock(p: Portfolio, ticker: String, amt_type: AmtType, amount: String): Box[BuyInfo] = {
	}

	def showUserInfo(u: User) : Box[UserInfo] = {
	}

	serveJx ( "api" / "v0" prefix {
		case "user"      :: user :: q Get  _ =>
			for {
				u <- User(user)
			} yield q match {
				case Nil => showUserInfo(user)
			}

		case "user"      :: user :: q Post _ =>
			for {
				u <- User(user)
			} yield q match {
				case "comment" :: trade_id :: Nil =>
					commentOnTrade(u, trade_id)
				case "join" :: league :: Nil =>
					joinLeague(u, league)
			}

		case "portfolio" :: league :: portfolio :: q Get _ =>
			for {
				p <- League(league).portfolio(portfolio)
			} yield q match {
				case Nil => showPortfolio(p)
			}

		case "portfolio" :: league :: portfolio :: q Post _ =>
			for {
				p <- League(league).portfolio(portfolio)
			} yield q match {
				case "buy" :: ticker :: amt_type :: amount :: Nil =>
					buyAsset(p, ticker, Asset(amt_type, amount))
				case "buy" :: asset_id :: Nil =>
					buyAsset(p, p.asset(asset_id))
				case "sell" :: asset_id :: Nil => 
					sellAsset(p, p.asset(asset_id), Amount())
				case "sell" :: asset_id :: amt_type :: amount :: Nil =>
					sellAsset(p, p.asset(asset_id), Amount(amt_type, amount))
			}
			
		case "leaderboard" :: Nil           Get _ => showLeaderboard()
		case "stock"       :: ticker :: Nil Get _ => showTickerInfo(ticker)
		case "trades"      :: Nil           Get _ => showRecentTrades()
	} )
}
