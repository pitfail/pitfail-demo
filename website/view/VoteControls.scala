
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

import model._
import model.schema._

object voteControls {
//

def apply(ref: Refreshable, ev: NewsEvent): NodeSeq = ev.asVotable match {
    case Some((buyerAside, sellerAside)) => controls(ref, ev, buyerAside, sellerAside)
    case _ => Nil
}

def controls(ref: Refreshable, ev: NewsEvent,
        buyerAside: DerivativeBuyerSetAside, sellerAside: DerivativeSellerSetAside) = {
    import control.LoginManager._
    import control.PortfolioSwitcher._
    
    val up = FormSubmit.rendered("Up") {
        try {
            currentPortfolio.userVoteUp(ev, buyerAside)
        }
        catch { case NotLoggedIn =>
            throw BadInput("You must be logged in to vote")
        }
        comet.News ! comet.Refresh
        ref.refresh
    }
    
    val down = FormSubmit.rendered("Down") {
        try {
            currentPortfolio.userVoteDown(ev, sellerAside)
        }
        catch { case NotLoggedIn =>
            throw BadInput("You must be logged in to vote")
        }
        comet.News ! comet.Refresh
        ref.refresh
    }
    
    <span>
        {up} ({priceSpec(-buyerAside.price * buyerAside.remaining)})
        {down} ({priceSpec(sellerAside.price * sellerAside.remaining)})
    </span>
}

def priceSpec(price: Dollars) =
    if (price < Dollars(0)) <span>pay {(-price).$}</span>
    else <span>receive {price.$}</span>
    
//
}

