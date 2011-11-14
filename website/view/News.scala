
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
import model.schema._

class News extends Refreshable
    with Loggable
{
    def registerWith = News
    
    def render = (in: NodeSeq) => readDB {
        <ul class="news"> {
            newsContents
        } </ul>
    }

    def newsContents = {
        val items = ( recentEvents(10) map {eventDescription(_)} ) map ( e =>
            <li class="event">{e}</li>
        )

        if (items nonEmpty) items
        else <li class="event no_items">[none]</li>
    }

    def eventDescription(ev: NewsEvent) =
        ev.action match {
            case Bought =>
                <span>
                    {UserLink(ev.subject.username)} bought
                    {ev.dollars.$} of {ev.ticker}
                </span>
            case Sold =>
                <span>
                    {UserLink(ev.subject.username)} sold
                    {ev.dollars.$} of {ev.ticker}
                </span>
            
            case other =>
                logger warn ("Don't know the event " + other)
                Nil
        }
}

object News extends RefreshHub

