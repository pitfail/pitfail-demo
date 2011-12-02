
package code
package snippet

import net.liftweb.{common, http}
import common._
import http._

import scala.xml._

import intform._
import errors._
import Box._

import formats._
import model.schema._

class SwitchPortfolio extends Page with Loggable {
    
    val nameParam = S param "name"
    
    def render = {
        import control.LoginManager._
        import control.PortfolioSwitcher._
 
        val user = currentUser
        
        try {
            nameParam match {
                case Full(key) => switchPortfolio(key)
                case _ => ()
            }
            
            myPage(currentUser)
        }
        catch {
            case NotLoggedIn => <p>You don't seemed to be logged in. How sad ;(;(;(</p>
        }
    }
}
