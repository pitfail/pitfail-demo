
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

object UserLink {
    def apply(username: String): NodeSeq =
        <a href={"/user?name=%s" format username}>{username}</a>
    
    def apply(user: model.schema.User): NodeSeq = apply(user.username)
}

