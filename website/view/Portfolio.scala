
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
import model.Schema._
import stockdata._
import org.joda.time.Duration

class Portfolio extends Refreshable with Loggable
{
    def registerWith = Portfolio
    
    def render = (in: NodeSeq) => trans {
        import control.LoginManager._
        
        for {
            user <-
                try { Some(currentUser) }
                catch {
                    case NotLoggedIn => None
                }
        }
        yield snippet.TChart(user, true).render
    }
    .getOrElse(<p>Login to view your portfolio</p>)
}

object Portfolio extends RefreshHub

