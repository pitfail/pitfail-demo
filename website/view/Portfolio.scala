
package code
package comet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
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
        
        this.logger.info("Rendering a portfolio for " + currentUser)
        
        try {
            <div class="block">
                <h2>Portfolio</h2>
                {snippet.tChart(currentUser, true)}
            </div>
        }
        catch {
            case NotLoggedIn =>
                <p>Login to view your portfolio</p>
        }
    }
    
}

object Portfolio extends RefreshHub

