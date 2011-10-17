
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

import matteform._

object AcceptOffer extends Loggable
{
    def render = "#yo" #> "no"
}

object DeclineOffer extends Loggable
{
    def render = "#yo" #> "no"
}

