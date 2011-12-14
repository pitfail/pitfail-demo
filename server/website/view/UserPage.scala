
// Written by: Owen Healy

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
        
        val curUser =
            try Some(currentUser)
            catch { case NotLoggedIn => None }
        
        if (curUser map (_ ~~ user) getOrElse false) myPage()
        else theirPage(user)
    }
    catch {
        case e: BadUser => <p>Sorry, {standardMessage(e)}</p>
    }
}

class MyPage {
    def render = myPage()
}

object myPage {
//

val dashboard = Dashboard()

def apply() =
    <lift:children>
        {Invites().render}
        <lift:comet type="Offers"/>
        {dashboard.render}
        <lift:comet type="OutgoingOffers"/>
    </lift:children>
    
//
}

object theirPage {
//
def apply(user: User) = {
    val ports = user.myPortfolios
    
    val portStuff = {
        if (ports.length == 0) {
            <p>This user has no portfolios.</p>
            <p>You should whine to them about that.</p>
        }
        else if (ports.length == 1) {
        }
        else {
            val portList = ports map { port =>
                <li>{PortfolioLink(port)}</li>
            }
            <ul>{portList}</ul>
        }
    }
    
    <div id="user-page" class="block">
        <h2>User: {user.username}</h2>
        {portStuff}
    </div>
}
//
}

object theirPortfolio {
//
def apply(port: Portfolio): NodeSeq = portfolio(port, None, false)
//
}

