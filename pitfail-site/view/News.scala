
package code
package comet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import lib.formats._
import snippet._

class News extends Refreshable
    with Loggable
{
    import model.Schema._
    
    def registerWith = News
    
    def render = (in: NodeSeq) => trans {
        <ul class="news"> {
            recentEvents(10) map event _
        } </ul>
    }
    
    def event(ev: NewsEvent) =
        ev.action match {
            case "buy" =>
                <li>{UserLink(ev.subject.username)} bought {ev.price.$} of {ev.ticker}</li>
            case "sell" =>
                <li>{UserLink(ev.subject.username)} sold {ev.price.$} of {ev.ticker}</li>
        }
}

object News extends RefreshHub

