
package code
package comet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml._
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import formats._
import intform._

import model._
import model.schema._
import stockdata._
import org.joda.time.Duration

class Dashboard extends Refreshable with Loggable
{
    // Eh
    def registerWith = schema.stockAssets
    
    def render = readDB {
        import control.LoginManager._
        import control.PortfolioSwitcher._
        
        try {
            val user = currentUser
            val ports = user.myPortfolios
            
            if (ports.length == 0) {
                <p>You have no portfolios. <a href="/create-portfolio">Create one</a>.</p>
            }
            else {
                val current = currentPortfolio
                
                val body = {
                    if (ports.length == 1) {
                        snippet.portfolio(currentPortfolio, Some(user), modifiable=true)
                    }
                    else {
                        val tabs = ports map { port =>
                            if (port ~~ current) {
                                <div class="tab active">
                                    <p>{port.name}</p>
                                </div>
                            }
                            else {
                                <div class="tab inactive">
                                    <p><a href={"/my-portfolio?name="+port.name}>{port.name}</a></p>
                                </div>
                            }
                        }
                        <div class="tab-bar">
                            {tabs}
                        </div>
                        <div class="tab-pane">
                            {snippet.portfolio(current, Some(user), modifiable=true)}
                        </div>
                    }
                }
                
                body
            }
        }
        catch {
            case NotLoggedIn =>
                <p>Login to view your portfolio</p>
        }
    }
}

