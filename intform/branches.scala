
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

class AggregateField[+A, F <: HList](
        constructor: F => A,
        fields: KList[Field, F]
    )
    extends Field[A]
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
        f: KList[Field, F]
    ): AggregateField[A,F] = new AggregateField(c, f)
        
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

// ---------------------------------------------------------------------------
// CaseField

class CaseField[+A](
        cases: Seq[Field[A]]
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
    ) = new CaseField(c)
}

