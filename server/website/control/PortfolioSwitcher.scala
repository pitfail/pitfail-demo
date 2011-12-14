
// Written by: Owen Healy

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
            case Some(key) =>
                try {
                    Portfolio byID key
                }
                catch { case NoSuchPortfolio =>
                    LoginManager.currentUser.aPortfolio
                }
            case None      =>
                val port = LoginManager.currentUser.lastPortfolio
                savedPortfolio := Some(port.id)
                port
        }
    
    def switchPortfolio(name: String) {
        val user = LoginManager.currentUser
        
        val port =
            try {
                user.portfolioByName(name)
            }
            catch { case NoSuchPortfolio =>
                user.aPortfolio
            }
        
        savedPortfolio := Some(port.id)
        user.userSwitchPortfolio(port)
    }
    
    def clearOnLogin() { savedPortfolio := None }
    
    object savedPortfolio extends SessionVar[Option[Key]](None)
}

