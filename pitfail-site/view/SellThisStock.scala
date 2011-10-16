
package code
package snippet

import code.comet._

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import scala.math.{BigDecimal}
import lib.formats._
import matteform._

object SellThisStock extends Loggable
{
    def render = form.render _
    
    object form extends Form[String](() => (),
        AttrField("ticker")
    )
    {
        def act(ticker: String) {
            userSellStock(ticker)
            
            NewsHub   ! Refresh
            Portfolio ! Refresh
        }
    }
    
    def userSellStock(ticker: String) {
        import control.LoginManager._
        import model.Schema._

        try {
            val user = currentUser
            user.mainPortfolio.sellAll(ticker)
        }
        catch {
            case NotLoggedIn =>
                throw BadInput("You must be logged in to trade")
                
            case DontOwnStock(_) =>
                throw BadInput("You don't own this stock")
        }
    }
}

