
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import intform._

class SellThisStock(ticker: String)
    extends Renderable
    with Loggable
{
    def render = FormSubmit.rendered("Sell") {
        import comet._
        
        userSellStock()
        
        Portfolio ! Refresh
        News ! Refresh
    }
    
    def userSellStock() {
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
object SellThisStock {
    def apply(t: String) = new SellThisStock(t).render
}

