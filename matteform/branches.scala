
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
    
    def produce() = (
        selected map {
            s => Right(cases(s).process)
        }
        getOrElse Left("None selected")
    )
    
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
    def produce() = Right(constructor(fields down mapProcess))
    
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
    
    val mapProcess = new (Field ~> Id) {
        def apply[T](field: Field[T]): T = field.process()
    }
}

// ------------------------------------------------------------
// ListField

class ListField[A](
    name: String,
    callback: () => Unit,
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
        //SHtml.onSubmitUnit(addOnePost _)
        //& ("name="+name+"Add [onclick]") #> SHtml.ajaxInvoke(addOneAjax _)
    )
    def produce() = Right(fields map (_.process) toList)
    
    def addOne() {
        fields.append(producer())
    }
    
    def addOnePost() {
        addOne()
        S.redirectTo(S.uri)
    }
    
    def addOneAjax(): JsCmd = {
        addOne()
        callback()
        Noop
    }
    
    def delete(i: Int) {
        fields.remove(i)
    }
    
    def deleteAjax(i: Int)(): JsCmd = {
        delete(i)
        callback()
        Noop
    }
    
    def clearInner() { fields.clear() }
}
object ListField {
    def apply[A](
        n: String,
        c: () => Unit,
        p: =>Field[A]
    ): Field[Seq[A]]
        = new ListField[A](n, c, () => p)
}

