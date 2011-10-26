
package code.snippet

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
        
        for {
            name <- nameParam ?~ "No user specified"
            user <- byUsername(name) ?~ ("No user named " + name)
        }
        yield {
            lazy val all =
                <div id="user-page">
                    {header}
                    {chart}
                </div>
            
            lazy val chart = TChart(user, false).render
            lazy val header = <h2>User: {name}</h2>
            
            all
        }
    }
    .withMessage(m => <p>Sorry,{m}</p>)
    
}

