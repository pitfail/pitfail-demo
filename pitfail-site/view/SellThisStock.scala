
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
    def render = doRender _
    
    def doRender(in: NodeSeq) = {
        logger.info("Rendering SellThisStock " + this)
        try {
            form.render(in)
        }
        catch { case e: Any =>
            logger.info("Caught " + e + " while rendering")
            throw e
        }
    }
    
    object form extends Form[String](() => (),
        AttrField("ticker")
    )
    {
        def act(ticker: String) {
            logger.info("Selling " + ticker + "!!")
            
            userSellStock(ticker)
            
            NewsHub   ! Refresh
            Portfolio ! Refresh
        }
    }
    
    def userSellStock(ticker: String) {
        import control.{SellStock => Seller}
        import Seller._
        import control.LoginManager.NotLoggedIn
        import comet.NewsHub

        try {
            Seller.userSellAllStock(ticker)
        }
        catch {
            case NotLoggedIn() =>
                throw BadInput("You must be logged in to trade")
                
            case DontOwnStock(_) =>
                throw BadInput("You don't own this stock")
        }
    }
}

