
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
        val field: Field[A]
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
    def apply[A, F <: HList, H <: HList] (
        c: F => A,
        f: FieldList[F, H]
    )(
        r: H => NodeSeq
    ): Form[A] =
        new Form(new AggregateField(c, f, r))
}

// ------------------------------------------------------------------
// Field

abstract class Field[+A]
    extends FieldRender
    with BasicErrors
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
object Field {
    implicit def toAdd[A](f: Field[A]) = new FieldAdd(f)
}

class FieldAdd[A](f: Field[A]) extends FieldListAdd {
    def unary_- = this
    
    // FieldListAdd
    type F2[+F<:HList] = A :+: F
    type H2[+H<:HList] = FieldRender :+: H
    
    def fieldListAdd[F1<:HList,H1<:HList](orig: FieldList[F1,H1])
        = FieldList[F2[F1], H2[H1]](
            fields = KCons(f, orig.fields),
            html   = HCons(f: FieldRender, orig.html)
        )
}

trait BasicErrors extends ErrorRender {
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
    with FieldListAdd
{
    def submitAjax() = {
        logger.info("Submitting!")
        
        val cmd = form().process() map callback getOrElse Noop
        form().refresh() & cmd
    }
    
    // FieldListAdd
    type F2[+F<:HList] = F
    type H2[+H<:HList] = FieldRender :+: H
    
    def fieldListAdd[F1<:HList,H1<:HList](orig: FieldList[F1,H1])
        = orig.copy(
            html = HCons(this: FieldRender, orig.html)
        )
    
    def unary_- = this
}
object Submit {
    def apply[A](form: =>Form[A], value: String)(callback: (A) => JsCmd) =
        new Submit(() => form, callback, value)
    
    def apply(text: String)(callback: =>JsCmd) = new SubmitRender
        with FormOuter with BasicErrors
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

