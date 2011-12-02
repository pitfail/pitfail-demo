
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
import scala.math._
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import intform._

import model._
import model.schema._

import control.LoginManager

class TestForm extends Page with Loggable {
    def render = <p>No</p>
}

