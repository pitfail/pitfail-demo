
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

import model._
import model.schema._

object PortfolioLink {
    def apply(port: Portfolio): NodeSeq = readDB {
        val name = port.name
        val wealth = port.spotValue
        
        val rank = port.rank
        val big = rank <= 10
        
        val standing = {
            if (big) <span>(#{rank} {wealth.$short})</span>
            else <span>({wealth.$short})</span>
        }
        
        if (big)
            <a class="big-name" href={"/portfolio?name=%s" format name}>{name} {standing}</a>
        else
            <a href={"/portfolio?name=%s" format name}>{name} {standing}</a>
    }
}


