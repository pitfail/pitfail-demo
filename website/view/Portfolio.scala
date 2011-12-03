
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

class Portfolio extends Refreshable with Loggable
{
    def registerWith = Portfolio
    
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
                        snippet.tChart(currentPortfolio, modifiable=true)
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
                            {snippet.tChart(current, modifiable=true)}
                        </div>
                    }
                }
                
                val sharing = {
                    val others = current.owners filter (o => !(o~~user))
                    this.logger.info("Shared with " + others)
                    if (others.length == 0) Nil
                    else {
                        val links = others map (snippet.UserLink(_))
                        val list = links reduceLeft { (a, b) =>
                            a ++ <span>, </span> ++ b
                        }
                        <span class="sharing">(Shared with {list})</span>
                    }
                }
                
                <div class="block">
                    <h2>Portfolio {sharing}</h2>
                    {body}
                </div>
            }
        }
        catch {
            case NotLoggedIn =>
                <p>Login to view your portfolio</p>
        }
    }
    
}

object Portfolio extends RefreshHub

