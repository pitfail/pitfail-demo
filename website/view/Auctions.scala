
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

class Auctions extends Refreshable
    with Loggable
{
    import model.Schema._
    
    def registerWith = News
    
    def render = (in: NodeSeq) => trans {
        <p>Coming soon...</p>
    }
}

object Auctions extends RefreshHub

