
package intform

import net.liftweb.{common, http, util}
import common.{Loggable, Logger}
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

// ref_727
abstract class Field[+A]
    extends BasicErrors
{
    def produce(): SubmitResult[A]
    
    // ref_997
    def process(): Option[A] = produce() match {
        case OK(a) =>
            error = None
            Some(a)
        case Error(msg) =>
            error = Some(msg)
            None
        case ChildError =>
            error = None
            None
    }
    
    def reset(): Unit
}

// ref_293
trait BasicErrors {
    var error: Option[String] = None

    def isError = error.isDefined

    def errorText: String = error getOrElse("")
    
    def runWithErrors(cmd: =>JsCmd): JsCmd =
        try {
            cmd
        }
        catch {
            case BadInput(msg) => {
                error = Some(msg)
                Noop
            }

            case BadFieldInput(cause, msg) => {
                cause.error = Some(msg)
                Noop
            }

            case e: LiftFlowOfControlException => throw e
            
            case e: Throwable => {
                error = Some("An unknown error occurred (see log messages)")
                new Logger { error("Error in form submission", e) }
                throw e
            }
        }
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
        val value: String,
        refresh: Boolean
    )
    extends SubmitRender
    with Loggable
    with FieldRender
    with BasicErrors
    with ErrorRender
{
    def submitAjax() = {
        logger.info("Submitting!")
        
        val cmd = runWithErrors {
            val result = form().process()
            result match {
                case Some(r) => logger.info("Resulted in " + r)
                case None    => logger.info("Failed do to input errors")
            }
            result map callback getOrElse Noop
        }
            
        if (refresh) form().refresh() & cmd
        else cmd
    }
}
object Submit {
    def apply[A](form: =>Form[A], value: String, refresh: Boolean=true)(callback: (A) => JsCmd) =
        new Submit(() => form, callback, value, refresh)
    
    def cancel(r: Refreshable, text: String)(callback: =>JsCmd) =
        new SubmitRender
            with BasicErrors
            with ErrorRender
    {
        val value = text
        def submitAjax() = runWithErrors(callback) & r.refresh()
    }
}

class FormSubmit(
        refresher: Refreshable,
        val value: String,
        callback: () => JsCmd
    )
    extends SubmitRender
    with BasicErrors
    with ErrorRender
    with FormOuter
    with Loggable
{
    def submitAjax() = runWithErrors(callback()) & refresher.refresh()
}
object FormSubmit {
    def apply(r: Refreshable, text: String)(callback: =>JsCmd) =
        new FormSubmit(r, text, () => callback)
    
    def apply(text: String)(callback: =>Unit) =
        new FormSubmit(Refreshable(Nil), text, () => {callback; Noop})
    
    def rendered(text: String)(callback: =>JsCmd) = {
        lazy val sub: FormSubmit = new FormSubmit(ref, text, () => callback)
        lazy val ref: Refreshable = Refreshable(
            sub.render ++ sub.errors
        )
        
        ref.render
    }
}

// ------------------------------------------------------------------
// Validation

case class BadInput(msg: String) extends Exception
case class BadFieldInput(cause: BasicErrors, msg: String) extends Exception

abstract class SubmitResult[+A]
case class OK[+A](res: A) extends SubmitResult[A]
case class Error(msg: String) extends SubmitResult[Nothing]
case object ChildError extends SubmitResult[Nothing]

