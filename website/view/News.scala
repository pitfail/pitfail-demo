
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
        val items = recentEvents(10) map { ev =>
            <li class="event">
                {News.eventDescription(ev, link=true)}
                ({News.link(ev,ev.numComments + " comments")})
            </li>
        }

        if (items nonEmpty) items
        else <li class="event no_items">[none]</li>
    }
}

object News extends RefreshHub {
    def eventDescription(ev: NewsEvent, link: Boolean) = {
        def alink(text: String) = if (link) News.link(ev,text) else text
    
        ev.action match {    
            case Bought(buyer, stock, shares, dollars, price) =>
                <span>{UserLink(buyer)} {alink("bought")} {dollars.$} of {stock}</span>
                
            case Sold(seller, stock, shares, dollars, price) =>
                <span>{UserLink(seller)} {alink("sold")} {dollars.$} of {stock}</span>
            
            case Offered(from, to, derivative, price) =>
                <span>{UserLink(from)} {alink("made an offer")} to {UserLink(to)}</span>
            
            case Accepted(from, to, derivative, price) =>
                <span>{UserLink(to)} {alink("accepted")} {UserLink(from)}'s offer</span>
                
            case Declined(from, to, derivative, price) =>
                <span>{UserLink(to)} {alink("declined")} {UserLink(from)}'s offer</span>
                
            case Auctioned(from, derivative, price) =>
                <span>{UserLink(from)} {alink("opened")} an auction</span>
                
            case Bid(from, on, price) =>
                <span>{UserLink(from)} {alink("bid")} on an auction</span>
            
            case Closed(user, offer) =>
                <span>{UserLink(user)} {alink("closed")} an auction</span>
                
            case Exercised(user, derivative) =>
                <span>{UserLink(user)} {alink("exercised")} a derivative</span>
                
            case other =>
                logger warn ("Don't know the event " + other)
                Nil
        }
    }
    
    def link(ev: NewsEvent, text: String) = <a href={"/event?id="+ev.id}>{text}</a>
}

