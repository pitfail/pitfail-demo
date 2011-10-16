
package matteform

import net.liftweb.{common, http, util}
import common.{Loggable,Logger}
import util._
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

case class BadInput(msg: String) extends Exception

sealed abstract class FieldResult[+A]
case class OK[+A](result: A) extends FieldResult[A]
case class Error(msg: String) extends FieldResult[Nothing]
case object ChildError extends FieldResult[Nothing]

