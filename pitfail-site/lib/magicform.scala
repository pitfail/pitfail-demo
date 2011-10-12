
package code
package lib

package object magicform {

import net.liftweb.{common, http, util}
import common.{Loggable,Logger}
import util._
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

// Type-level metaprogramming
// http://apocalisp.wordpress.com/2010/11/03/type-level-programming-in-scala-part-8b-klist%C2%A0basics/
import up._
import HList._
import KList._
import ~>._

import scala.collection.mutable.ListBuffer
import java.util.UUID
import lib.BadInput
import lib.cssfuncs._

abstract class Form[A](
    val state: StatefulSnippet,
    val field: Field[A]
    )
    extends Loggable
{
    val errorName = UUID.randomUUID.toString
    
    def render: CssBindFunc =
        (
              "name=submit"  #> SHtml.onSubmitUnit(process _)
            & "#submitError" #> liftMsg(errorName)
            & field.render
        )
        
    def process() {
        field.produce match {
            case InputError(error) => reportErrors(error)
            case SubmitOK(result)  =>
                try {
                    act(result)
                    field.clear()
                }
                catch { case BadInput(msg) =>
                    S.error(errorName, msg)
                }
        }
    
        state.redirectTo(S.uri)
    }
            
    def reportErrors(errors: Seq[FieldError]) {
        errors foreach { case FieldError(name, msg) =>
            S.error(name, msg)
        }
    }
    
    def act(result: A): Unit
}

abstract class Field[+A](val name: String)
    extends Loggable
{
    val errorName = UUID.randomUUID.toString
    
    def renderInner: CssBindFunc
    def produce(): SubmitResult[A]
    def clear(): Unit
    
    def inputError(msg: String) = InputError(errorName, msg)
    
    def render: CssBindFunc = 
        renderInner & ("#"+name+"Error") #> liftMsg(errorName)
}

// Visible error messages for the user
def liftMsg(id: String) =
    <lift:Msg id={id} errorClass="inputError"/>

sealed abstract class SubmitResult[+A]

case class SubmitOK[+A](
    val result: A
)
    extends SubmitResult[A]

case class InputError(
    errors: Seq[FieldError]
)
    extends SubmitResult[Nothing]
{ }
object InputError {
    def apply(e: FieldError): InputError = InputError(e :: Nil)
    def apply(n: String, m: String): InputError
        = InputError(FieldError(n, m))
}

case class FieldError(
    name: String,
    message: String
)

trait TextField
{
    val name: String
    val initText: String
    
    var text: String = initText
    
    def renderInner =
        ("name="+name) #> (
              "input [value]" #> text
            & "input"         #> SHtml.onSubmit(text = _)
        )
    
    def clear() { text = initText }
}

class StringField(
    name: String,
    val initText: String
    )
    extends Field[String](name)
    with TextField
{
    def produce() = SubmitOK(text)
}
object StringField {
    def apply(n: String, i: String): Field[String]
        = new StringField(n, i)
}

class NumberField(
    name: String,
    val initText: String
    )
    extends Field[BigDecimal](name)
    with TextField
{
    def produce() =
        try {
            SubmitOK(BigDecimal(text))
        }
        catch {
            case e: NumberFormatException => inputError("Should be a number")
        }
}
object NumberField {
    def apply(n: String, i: String): Field[BigDecimal]
        = new NumberField(n, i)
}

class HiddenField(
    name: String
    )
    extends Field[String](name)
{
    var text: Option[String] = None
    
    def renderInner = ("#"+name) #> SHtml.onSubmit(t => text = Some(t))
    
    def produce() = text match {
        case Some(text) => SubmitOK(text)
        case _ =>
            throw new IllegalStateException(name+" should have been set")
    }
    
    def clear() { }
}
object HiddenField {
    def apply(name: String): Field[String]
        = new HiddenField(name)
}

class AttrField(
    name: String
    )
    extends Field[String]("noname")
    with Loggable
{
    var text: Option[String] = None
    
    def renderInner = {
        val attr = S.attr(name) openOr {
            throw new IllegalStateException(name+" attribute needs to be set")
        }
        "name=yuck" #>
            SHtml.onSubmitUnit { () => text = Some(attr) }
    }
    
    def produce() = text match {
        case Some(text) => SubmitOK(text)
        case _ =>
            throw new IllegalStateException(name+" should have been set")
    }
    
    def clear() { }
}
object AttrField {
    def apply(name: String): Field[String]
        = new AttrField(name)
}

class ConstField[+A](
    val it: A
    )
    extends Field[A]("noname")
{
    def renderInner = same
    def produce() = SubmitOK(it)
    def clear() { }
}
object ConstField {
    def apply[A](it: A): Field[A] = new ConstField(it)
}

class CaseField[+A](
    name: String,
    val cases: Map[String,Field[A]]
    )
    extends Field[A](name)
{
    var selected: Option[String] = None
    
    def renderInner = {
        val radios = SHtml.radio(
            cases.keys toList,
            selected,
            t => selected = Some(t)
        )
        
        val stages = cases map { case (n, c) =>
            ("#"+name+"_"+"case_"+n) #> (
                  ("#"+n) #> radios(n)
                & c.render
            )
        }
        
        stages.foldLeft(same)(_ & _)
    }
    
    def produce() = selected map {s =>
        cases(s).produce
    } getOrElse InputError(name, "None selected")
    
    def clear() {
        selected = None
        cases foreach { case (_, f) => f.clear() }
    }
}
object CaseField {
    def apply[A](n: String, cs: (String,Field[A])*): Field[A] =
        new CaseField(n, cs toMap)
}

class AggregateField[+A, HL <: HList](
    val constructor: HL => A,
    val fields: KList[Field, HL]
    )
    extends Field[A]("submit")
{
    import AggregateField._
    
    def renderInner = fields.toList.foldLeft(same)(_ & _.render)
    def produce() = mapCollect(fields map mapProduce) match {
        case SubmitOK(hl)      => SubmitOK(constructor(hl))
        case e @ InputError(_) => e
    }
    
    def clear() {
        fields.toList foreach (_.clear())
    }
}
object AggregateField {
    def apply[A, HL <: HList](
        c: HL => A,
        f: KList[Field, HL]
    ): Field[A]
        = new AggregateField(c, f)
    
    val mapProduce = new (Field ~> SubmitResult) {
        def apply[T](field: Field[T]): SubmitResult[T] = field.produce()
    }
    
    val mapExtract = new (SubmitResult ~> Id) {
        def apply[T](field: SubmitResult[T]): T = assumeOK(field)
    }
    
    def mapCollect[HL <: HList](reses: KList[SubmitResult, HL]):
        SubmitResult[HL] =
    {
        val allOK = reses.toList forall {
            case SubmitOK(_) => true
            case _ => false
        }
        
        if (allOK) SubmitOK(reses down mapExtract)
        else InputError(reses.toList flatMap extractErrors)
    }
    
    def assumeOK[A](res: SubmitResult[A]): A = res match {
        case SubmitOK(a) => a
        case _ =>
            throw new IllegalStateException("Blame Owen on this one")
    }
    
    def extractErrors(res: SubmitResult[_]): Seq[FieldError] = res match {
        case InputError(e) => e
        case _ => Seq()
    }
}

class ListField[A](
    val state: StatefulSnippet,
    name: String,
    val producer: () => Field[A]
)
    extends Field[Seq[A]](name)
    with Loggable
{
    import ListField._
    
    val fields: ListBuffer[Field[A]] = ListBuffer()
    
    def renderInner = (
          ("#"+name)           #> (fields map (_.render))
        & ("name="+name+"Add") #> SHtml.onSubmitUnit(addOne _)
    )
    def produce() = collectFields(fields map (_.produce) toList)
    
    def addOne() {
        fields.append(producer())
        state.redirectTo(S.uri)
    }
    
    def clear() { fields.clear() }
}
object ListField {
    def apply[A](
        s: StatefulSnippet,
        n: String,
        p: =>Field[A]
    ): Field[Seq[A]]
        = new ListField[A](s, n, () => p)
    
    def collectFields[A](fs: List[SubmitResult[A]]): SubmitResult[Seq[A]] =
        fs match {
            case Nil => SubmitOK(Nil)
            case head :: tail =>
                (head, collectFields(tail)) match {
                    case (SubmitOK(a), SubmitOK(as))      => SubmitOK(a +: as)
                    case (SubmitOK(a), InputError(e))     => InputError(e)
                    case (InputError(e), SubmitOK(as))    => InputError(e)
                    case (InputError(e1), InputError(e2)) => InputError(e1 ++ e2)
                }
        }
}

val same: CssBindFunc = "#foo" #> "foo"

}

