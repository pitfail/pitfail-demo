
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

// Type-level metaprogramming
// http://apocalisp.wordpress.com/2010/11/03/type-level-programming-in-scala-part-8b-klist%C2%A0basics/
import up._
import HList._
import KList._
import ~>._

// Capitalized Scalaz has the implicits
import scalaz.Scalaz
import Scalaz.{Id=>_, _}

import scala.collection.mutable.ArrayBuffer

// ------------------------------------------------------------
// CaseField

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
    
    def produce() = selected match {
        case Some(i) =>
            cases(i).process() match {
                case Some(ans) => OK(ans)
                case None      => ChildError
            }
        case None =>
            Error("None selected")
    }
    
    def clearInner() {
        selected = None
        cases foreach { case (_, f) => f.clear() }
    }
}
object CaseField {
    def apply[A](n: String, cs: (String,Field[A])*): Field[A] =
        new CaseField(n, cs toMap)
}

// ------------------------------------------------------------
// AggregateField

class AggregateField[+A, HL <: HList](
    val constructor: HL => A,
    val fields: KList[Field, HL]
    )
    extends Field[A]("submit")
{
    import AggregateField._
    
    def renderInner = fields.toList.foldLeft(same)(_ & _.render)
    def produce() = {
        val inners = fields map mapProcess
        
        if (inners.toList forall (_.isDefined))
            OK(constructor(inners down mapExtract))
        else
            ChildError
    }
    
    def clearInner() {
        fields.toList foreach (_.clear())
    }
}
object AggregateField {
    def apply[A, HL <: HList](
        c: HL => A,
        f: KList[Field, HL]
    ): Field[A]
        = new AggregateField(c, f)
    
    val mapProcess = new (Field ~> Option) {
        def apply[T](field: Field[T]): Option[T] = field.process()
    }
    
    val mapExtract = new (Option ~> Id) {
        def apply[T](o: Option[T]): T = o match {
            case Some(t) => t
            case _ =>
                throw new IllegalStateException(
                    "Owen doesn't know how to use types!"
                )
        }
    }
}

// ------------------------------------------------------------
// ListField

class ListField[A](
    name: String,
    snippet: StatefulSnippet,
    val producer: () => Field[A]
)
    extends Field[Seq[A]](name)
    with Loggable
{
    val fields: ArrayBuffer[Field[A]] = ArrayBuffer()
    
    def renderInner = (
          ("#"+name) #> (fields.indices map ( i =>
                "name=delete" #> { del =>
                    SHtml.ajaxSubmit((del\"@value").text, deleteAjax(i) _)
                }
              & fields(i).render
          ))
        & ("name="+name+"Add") #> { add =>
            SHtml.ajaxSubmit((add\"@value").text, addOneAjax _)
        }
    )
    def produce() =
        (fields map (_.process)).sequence match {
            case Some(a) => OK(a)
            case None    => ChildError
        }
    
    def addOne() {
        fields.append(producer())
    }
    
    def addOnePost() {
        addOne()
        S.redirectTo(S.uri)
    }
    
    def addOneAjax(): JsCmd = {
        addOne()
        Noop
    }
    
    def delete(i: Int) {
        fields.remove(i)
    }
    
    def deleteAjax(i: Int)(): JsCmd = {
        delete(i)
        Noop
    }
    
    def clearInner() { fields.clear() }
}
object ListField {
    def apply[A](
        n: String,
        s: StatefulSnippet,
        p: =>Field[A]
    ): Field[Seq[A]]
        = new ListField[A](n, s, () => p)
}

