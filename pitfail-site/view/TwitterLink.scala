
package code.snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

object TwitterLink extends Loggable {
    def render = "a [href]" #> "/twitter/login"
}

