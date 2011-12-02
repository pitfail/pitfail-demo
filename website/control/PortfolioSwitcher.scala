
package code
package control

import net.liftweb.{common, http, util}
import common._
import util._
import http._
import sessions._

import model._
import model.schema._

object PortfolioSwitcher extends Loggable {
    
    def currentPortfolio: Portfolio =
        savedPortfolio.is match {
            case Some(key) => Portfolio byID key
            case None      =>
                val port = LoginManager.currentUser.lastPortfolio
                savedPortfolio := Some(port.id)
                port
        }
    
    def switchPortfolio(name: String) {
        val user = LoginManager.currentUser
        
        val port = user.portfolioByName(name)
        savedPortfolio := Some(port.id)
        
        user.userSwitchPortfolio(port)
    }
    
    def clearOnLogin() { savedPortfolio := None }
    
    object savedPortfolio extends SessionVar[Option[Key]](None)
}

