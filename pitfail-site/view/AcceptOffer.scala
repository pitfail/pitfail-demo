
package code
package snippet

import code.comet._

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import scala.math.{BigDecimal}
import lib.formats._
import matteform._

object AcceptOffer extends Loggable
{
    def render = form.render _
    
    object form extends Form[String](() => (),
        AttrField("offerId")
    )
    {
        def act(offerId: String) {
            logger.info("Accepting " + offerId + "!!")
        }
    }
}

object DeclineOffer extends Loggable
{
    def render = form.render _
    
    object form extends Form[String](() => (),
        AttrField("offerId")
    )
    {
        def act(offerId: String) {
            logger.info("Declining " + offerId + "!!")
        }
    }
}

