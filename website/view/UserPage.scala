
package code.snippet

import net.liftweb.{common, http}
import common._
import http._

import intform._
import errors._
import Box._

import model.Schema._

class UserPage extends Page with Loggable {
    
    def render = trans {
        import model.Schema._
        
        for {
            name <- (S attr "name")  ?~ "No user specified"
            user <- byUsername(name) ?~ ("No user named " + name)
        }
        yield TChart(user, false).render
    }
    .withMessage(m => <p>Sorry,{m}</p>)
    
}

