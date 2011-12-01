
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

class UserPage extends Page with Loggable {
    
    val nameParam = S param "name"
    
    def render = try readDB {
        import control.LoginManager._
        
        val name = nameParam openOr {
            throw NoSuchUser
        }
        val user = User byName name
        
        val chart = tChart(user, modifiable=false)
        
        val them =
            <div id="user-page" class="block">
                <h2>User: {name}</h2>
                {chart}
            </div>
        
        val us =
            <lift:comet type="Offers"/> ++
            <lift:comet type="Portfolio"/> ++
            <lift:comet type="OutgoingOffers"/>
            
        val curUser =
            try Some(currentUser)
            catch { case NotLoggedIn => None }
        
        if (curUser map (_ ~~ user) getOrElse false) {
            this.logger.info("Showing page for US")
            us
        }
        else {
            this.logger.info("Showing page for THEM")
            them
        }
    }
    catch {
        case e: BadUser => <p>Sorry, {standardMessage(e)}</p>
    }
}

