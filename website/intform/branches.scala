
package intform

import net.liftweb.{common, http, util}
import common.Loggable
import util._
import scala.xml._
import http._
import Helpers._
import java.util.UUID

import up._
import HList._
import KList._
import ~>._
import scalaz.Scalaz._

// ---------------------------------------------------------------------------
// AggregateField

class AggregateField[+A, F <: HList](
        constructor: F => A,
        fields: KList[Field, F],
        val renderer: () => NodeSeq
    )
    extends Field[A]
    with AggregateRender
{
    import AggregateField._
    
    def produce() = {
        val inners = fields map mapProcess
        
        if (inners.toList forall (_.isDefined))
            OK(constructor(inners down mapExtract))
        else
            ChildError
    }
    
    def reset() { fields.toList map (_.reset) }
}
object AggregateField {
    def apply[A, F <: HList](
        c: F => A,
        f: KList[Field, F],
        r: => NodeSeq
    ): AggregateField[A,F] = new AggregateField(c, f, () => r)
        
    val mapProcess = new (Field ~> Option) {
        def apply[T](field: Field[T]): Option[T] = field.process()
    }
    
    val mapExtract = new (Option ~> (~>.Id)) {
        def apply[T](o: Option[T]): T = o match {
            case Some(t) => t
            case _ =>
                throw new IllegalStateException(
                    "Owen doesn't know how to use types!"
                )
        }
    }
}

// ---------------------------------------------------------------------------
// CaseField

class CaseField[+A](
        cases: Seq[Field[A]],
        val renderer: CaseChoices => NodeSeq
    )
    extends Field[A]
    with Loggable
    with CaseRender
{
    def produce() = selected match {
        case Some(name) =>
            table(name).process() match {
                case Some(res) => OK(res)
                case None      => ChildError
            }
        case _ => Error("None selected")
    }
    
    def reset() {
        selected = None
        cases map (_.reset)
    }
    
    var selected: Option[String] = None
    
    val table = cases map { f =>
        (UUID.randomUUID.toString, f)
    } toMap
    
}
object CaseField {
    def apply[A](
        c: Seq[Field[A]],
        r: CaseChoices => NodeSeq
    ) = new CaseField(c, r)
}

// ---------------------------------------------------------------------------
// ListField

class ListField[A](
        val generator: () => Field[A] with Renderable,
        val renderer: (Seq[ItemRender], NodeSeq) => NodeSeq
    )
    extends Field[Seq[A]]
    with ListRender
{
    import collection.mutable.ArrayBuffer
    
    val items: ArrayBuffer[Field[A] with Renderable] = ArrayBuffer()
    
    def addOne() { items append generator()  }
    def deleteOne(n: Int) { items remove n  }
    def reset() { items.clear }
    def produce() =
        (items map (_.process)).sequence match {
            case Some(a) => OK(a)
            case None    => ChildError
        }
}
object ListField {
    def apply[A](
            g: => Field[A] with Renderable,
            r: (Seq[ItemRender], NodeSeq) => NodeSeq
        )
        = new ListField[A](() => g, r)
}

