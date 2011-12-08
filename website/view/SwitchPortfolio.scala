
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
 
        try {
            val user = currentUser
            
            nameParam match {
                case Full(key) => switchPortfolio(key)
                case _ => ()
            }
            
            myPage()
        }
        catch {
            case NotLoggedIn     => S.redirectTo("/")
            case NoSuchPortfolio => 
                <div class="block page-error">
                    <p>This portfolio does not exist</p>
                </div>
        }
    }
}
