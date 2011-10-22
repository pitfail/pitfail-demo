
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

// ---------------------------------------------------------------------------
// AggregateField

class AggregateField[+A, F <: HList, H <: HList](
        constructor: F => A,
        fields: FieldList[F, H],
        renderer: H => NodeSeq
    )
    extends Field[A]
{
    import AggregateField._
    
    def produce() = {
        val inners = fields.fields map mapProcess
        
        if (inners.toList forall (_.isDefined))
            OK(constructor(inners down mapExtract))
        else
            ChildError
    }
    
    def reset() { fields.fields.toList map (_.reset) }
    
    def main: NodeSeq = renderer(fields.html)
}
object AggregateField {
    def apply[A, F <: HList, H <: HList] (
        c: F => A,
        f: FieldList[F, H]
    )(
        r: H => NodeSeq
    ) =
        new AggregateField(c, f, r)
        
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

case class FieldList[+F <: HList, +H <: HList](
        fields: KList[Field, F],
        html: H
    )
object FieldList {
    def empty = FieldList(KNil, new HNil)
}

trait FieldListAdd {
    type F2[+F<:HList] <: HList
    type H2[+H<:HList] <: HList
    
    def fieldListAdd[F1<:HList,H1<:HList](orig: FieldList[F1,H1]): FieldList[F2[F1], H2[H1]]
    def a[F1<:HList,H1<:HList](orig: FieldList[F1,H1]) = fieldListAdd(orig)
}

// ---------------------------------------------------------------------------
// CaseField

class CaseField[+A](
        cases: Seq[Field[A]],
        val renderer: List[ChoiceRender] => NodeSeq
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
        c: Seq[Field[A]]
    )(
        r: List[ChoiceRender] => NodeSeq
    ) = new CaseField(c, r)
}

class ChoiceRender(
        f: FieldRender,
        radio: NodeSeq
    )
    extends FieldRender
{
    def choice = radio
    def main = f.main
    def errors = f.errors
}

