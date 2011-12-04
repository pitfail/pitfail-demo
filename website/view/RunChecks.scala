
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

import intform._
import control.Checker

class RunChecks {
    def render = FormSubmit.rendered("Run Checks") {
        Checker.runChecks()
    }
}

