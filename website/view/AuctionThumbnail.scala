
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

class AuctionThumbnail extends Refreshable
    with Loggable
{
    import model.Schema._
    
    def registerWith = News
    
    def render = (in: NodeSeq) => trans {
        val auctions = recentAuctions(5)
        
        val items = auctions map { auc =>
            val from  = auc.offerer.owner.username
            val price = auc.goingPrice.$
            
            val url = "/auction?id=%s" format auc.id
            
            <li><a href={url}>from {from} for {price}</a></li>
        }
        
        <ul>{items}</ul>
    }
}

object AuctionThumbnail extends RefreshHub

