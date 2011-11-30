
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

class RefreshHack extends intform.Page with Loggable
{
    def render = {
        import comet._
        
        Portfolio        ! Refresh
        News             ! Refresh
        AuctionThumbnail ! Refresh
        Offers           ! Refresh
        OutgoingOffers   ! Refresh
        
        Nil: NodeSeq
    }
}


