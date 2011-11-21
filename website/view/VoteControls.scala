
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

object voteControls {
//

def apply(ev: NewsEvent): NodeSeq = ev match {
    case Accepted(buyerAside=b, sellerAside=s) => controls(b, s)
    case _ => Nil
}

def controls(buyerAside: DerivativeBuyerSetAside, sellerAside: DerivativeSellerSetAside) = {
    val up = FormSubmit.rendered("Up") {
        currentUser.userVoteUp(buyerAside)
    }
    
    val down = FormSubmit.rendered("Down") {
        currentUser.userVoteDown(sellerAside)
    }
    
    <span>{up} {down}</span>
}

//
}

