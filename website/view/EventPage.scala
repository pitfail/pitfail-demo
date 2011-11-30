
package code
package snippet

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
import intform._
import errors._

import model.schema._

class EventPage extends Page with Loggable {
    def render = refreshable.render
    val param = S param "id"
    
    val refreshable = Refreshable(doRender)
        
    def doRender: NodeSeq = param map eventPage _ openOr (
        <p>This event does not seem to exist</p>
    )
    
    def eventPage(id: String): NodeSeq = try {
        import comet.News._
        
        val ev = NewsEvent byID id
        
        <div id="event" class="block">
            <h3 class="event"><span class="eventDate">{ev.when.toNearbyString}</span>:
                {eventDescription(ev, link=false)}
                {voteTally(ev)}
                {voteControls(refreshable, ev)}
            </h3>
                
            {commentPage(ev)}
        </div>
    }
    catch { case _: BadUser =>
        <p>This event seems to not exist</p>
    }
    
    def voteTally(ev: NewsEvent) =
        if (ev.isVotable)
            <span class="long-tally">{ev.buyerTally} Up / {ev.sellerTally} Down</span>
        else
            Nil
}


