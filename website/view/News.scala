
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
    def registerWith = newsHub
    
    def render = (in: NodeSeq) => readDB {
        <ul class="news"> {
            newsContents
        } </ul>
    }

    def newsContents = {
        val items = recentEvents(10) map { ev =>
            <li class="event">
                {News.eventBriefTally(ev)}
                {News.eventDescription(ev, link=true)}
                ({News.link(ev,ev.numComments + " comments")})
            </li>
        }

        if (items nonEmpty) items
        else <li class="event no_items">[none]</li>
    }
}

object News extends Loggable {
    def eventDescription(ev: NewsEvent, link: Boolean) = {
        def alink(text: String) = if (link) News.link(ev,text) else text
    
        ev.action match {    
            case Bought(buyer, stock, shares, dollars, price) =>
                <span>{PortfolioLink(buyer)} {alink("bought")} {dollars.$} of {stock}</span>
                
            case Sold(seller, stock, shares, dollars, price) =>
                <span>{PortfolioLink(seller)} {alink("sold")} {dollars.$} of {stock}</span>
            
            case Offered(from, to, derivative, price) =>
                <span>{PortfolioLink(from)} {alink("made an offer")} to {PortfolioLink(to)}</span>
            
            case Accepted(from, to, derivative, price, _, _) =>
                <span>{PortfolioLink(to)} {alink("accepted")} {PortfolioLink(from)}'s offer</span>
                
            case Declined(from, to, derivative, price) =>
                <span>{PortfolioLink(to)} {alink("declined")} {PortfolioLink(from)}'s offer</span>
                
            case Auctioned(from, derivative, price) =>
                <span>{PortfolioLink(from)} {alink("opened")} an auction</span>
                
            case Bid(from, on, price) =>
                <span>{PortfolioLink(from)} {alink("bid")} on an auction</span>
            
            case Closed(port, offer) =>
                <span>{PortfolioLink(port)} {alink("closed")} an auction</span>
                
            case Exercised(port, derivative) =>
                <span>{PortfolioLink(port)} {alink("exercised")} a derivative</span>
                
            case BuyOrdered(port, ticker, shares, limit) =>
                <span>{PortfolioLink(port)} {alink("ordered")} {shares.###()}
                    shares of {ticker} at {limit.$}/sh</span>
                    
            case SellOrdered(port, ticker, shares, limit) =>
                <span>{PortfolioLink(port)} {alink("offered")} {shares.###()}
                    shares of {ticker} at {limit.$}/sh</span>
                
            case other =>
                logger warn ("Don't know the event " + other)
                Nil
        }
    }
    
    def link(ev: NewsEvent, text: String) = <a href={"/event?id="+ev.id}>{text}</a>
    
    def eventBriefTally(ev: NewsEvent) =
        if (ev.isVotable)
            <span class="brief-tally">+{ev.buyerTally}/-{ev.sellerTally}</span>
        else
            Nil
}

