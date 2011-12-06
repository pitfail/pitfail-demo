
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

class AuctionThumbnail extends Refreshable
    with Loggable
{
    def registerWith = auctionOffers

    def render = (in: NodeSeq) => readDB {
        val auctions = recentAuctions(5)
        
        val items = auctions map { auc =>
            val from  = auc.offerer
            val price = auc.goingPrice.$
            
            val url = "/auction?id=%s" format auc.id
            
            <li><a href={url}>from {PortfolioLink(from)} for {price}</a></li>
        }

        if (items nonEmpty) {
            <ul>{items}</ul>
        } else {
            <ul class="no_items">[none]</ul>
        }
    }
}

