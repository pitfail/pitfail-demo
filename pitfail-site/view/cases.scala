
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

object cases {
    def render(in: NodeSeq): NodeSeq = {
        val choice = S.attr("choice") openOr {
            throw new IllegalStateException("Specify a choice")
        }
        
        val trans = ("test=%s" format choice) #> { (nodes: NodeSeq) =>
            nodes flatMap (_.child)
        }
        trans(in)
    }
}

