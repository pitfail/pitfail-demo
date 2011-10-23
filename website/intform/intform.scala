
package intform

import net.liftweb.{common, http, util}
import common.Loggable
import util._
import scala.xml._
import http._
import Helpers._
import java.util.UUID

import js._
import JsCmds._
import JE._

import scalaz.Scalaz._
import up._
import HList._
import KList._
import ~>._

// ------------------------------------------------------------------
// Form

class Form[+A](
        val field: Field[A] with Renderable
    )
    extends Processable[A]
    with InnerFieldRender
    with FormOuter
    with Refreshable
{
    def process(): Option[A] = field.process()
    def reset() { field.reset() }
}
object Form {
    def apply[A, F <: HList](
        c: F => A,
        f: KList[Field, F],
        r: => NodeSeq
    ): Form[A] = new Form(new AggregateField(c, f, ()=>r))
}

// ------------------------------------------------------------------
// Field

abstract class Field[+A]
    extends BasicErrors
{
    def produce(): SubmitResult[A]
    def process(): Option[A] = produce() match {
        case OK(a) =>
            errorText = ""
            Some(a)
        case Error(msg) =>
            errorText = msg
            None
        case ChildError =>
            errorText = ""
            None
    }
    
    def reset(): Unit
}

trait BasicErrors {
    var errorText: String = ""
}

// ------------------------------------------------------------------
// Submit

trait Processable[+A] {
    def process(): Option[A]
}

object UnitProcessable extends Processable[Unit] {
    def process() = Some(())
}

class Submit[A](
        form: () => Processable[A] with Refreshable,
        callback: (A) => JsCmd,
        val value: String
    )
    extends SubmitRender
    with Loggable
    with FieldRender
    with BasicErrors
    with ErrorRender
{
    def submitAjax() = {
        logger.info("Submitting!")
        
        val cmd = 
            try {
                val result = form().process()
                result match {
                    case Some(r) => logger.info("Resulted in " + r)
                    case None    => logger.info("Failed do to input errors")
                }
                result map callback getOrElse Noop
            }
            catch {
                case BadInput(msg) =>
                    logger.info("Bad input: " + msg)
                    errorText = msg
                    Noop
                case e: Any =>
                    logger.error("Unhandled error in submission: " + e)
                    errorText = "An unknown error occurred (see log messages)"
                    Noop
            }
            
        form().refresh() & cmd
    }
}
object Submit {
    def apply[A](form: =>Form[A], value: String)(callback: (A) => JsCmd) =
        new Submit(() => form, callback, value)
    
    def apply(text: String)(callback: =>JsCmd) = new SubmitRender
        with FormOuter with BasicErrors with ErrorRender
    {
        val value = text
        def submitAjax() = callback
    }.render
}

// ------------------------------------------------------------------
// Validation

case class BadInput(msg: String) extends Exception

abstract class SubmitResult[+A]
case class OK[+A](res: A) extends SubmitResult[A]
case class Error(msg: String) extends SubmitResult[Nothing]
case object ChildError extends SubmitResult[Nothing]
