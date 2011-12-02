
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

class PortfolioPage extends Page with Loggable {
    
    val nameParam = S param "name"
    
    def render = try readDB {
        import control.LoginManager._
        
        val name = nameParam openOr {
            throw NoSuchUser
        }
        val port = Portfolio byName name
        
        val curUser =
            try Some(currentUser)
            catch { case NotLoggedIn => None }
        
        curUser match {
            case Some(user) if port.isOwnedBy(user) =>
                myPage(user)
                
            case _ =>
                theirPortfolio(port)
        }
    }
    catch {
        case e: BadUser => <p>Sorry, {standardMessage(e)}</p>
    }
}


