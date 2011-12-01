
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

object UserLink {
    def apply(user: User): NodeSeq = readDB {
        val username = user.username
        val wealth = user.mainPortfolio.spotValue
        
        val rank = user.mainPortfolio.rank
        val big = rank <= 10
        
        val standing = {
            if (big) <span>(#{rank} {wealth.$short})</span>
            else <span>({wealth.$short})</span>
        }
        
        if (big)
            <a class="big-name" href={"/user?name=%s" format username}>{username} {standing}</a>
        else
            <a href={"/user?name=%s" format username}>{username} {standing}</a>
    }
}

