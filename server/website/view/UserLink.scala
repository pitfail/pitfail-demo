
// Written by: Owen Healy

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
        
        <a href={"/user?name=%s" format username}>{username}</a>
    }
}

