
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

import formats._
import snippet._

class News extends Refreshable
    with Loggable
{
    import model.Schema._
    
    def registerWith = News
    
    def render = (in: NodeSeq) => trans {
        <ul class="news"> {
            recentEvents(10) map ( ev =>
                <li class="event">{event(ev)}</li>
            )
        } </ul>
    }
    
    def event(ev: NewsEvent) =
        ev.action match {
            case "buy" =>
                <span>
                    {UserLink(ev.subject.username)} bought
                    {ev.dollars.$} of {ev.ticker}
                </span>
            case "sell" =>
                <span>
                    {UserLink(ev.subject.username)} sold
                    {ev.dollars.$} of {ev.ticker}
                </span>
        }
}

object News extends RefreshHub

