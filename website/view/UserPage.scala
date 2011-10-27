
package code
package snippet

import net.liftweb.{common, http}
import common._
import http._

import scala.xml._

import intform._
import errors._
import Box._

import model.Schema._

class UserPage extends Page with Loggable {
    
    val nameParam = S param "name"
    logger.info("Name is: " + nameParam)
    
    def render = trans {
        import model.Schema._
        import control.LoginManager._
        
        for {
            name <- nameParam ?~ "No user specified"
            user <- byUsername(name) ?~ ("No user named " + name)
        }
        yield {
            lazy val them =
                <div id="user-page">
                    {header}
                    {chart}
                </div>
            
            lazy val chart = TChart(user, false).render
            lazy val header = <h2>User: {name}</h2>
            
            lazy val us =
                <lift:comet type="Offers"/> ++
                <lift:comet type="Portfolio"/> ++
                <lift:comet type="OutgoingOffers"/>
            
            lazy val curUser =
                try Some(currentUser)
                catch { case NotLoggedIn => None }
            
            if (curUser map (_ == user) getOrElse false)
                us
            else them
        }
    }
    .withMessage(m => <p>Sorry,{m}</p>)
    
}

