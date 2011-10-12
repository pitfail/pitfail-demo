
package code
package snippet

import net.liftweb._
import common._
import http._
import util._
import js._
import JsCmds._
import JE._
import Helpers._

object OpenIDLink extends Loggable {
    def render = "a [href]" #> "/openid/login"
}

