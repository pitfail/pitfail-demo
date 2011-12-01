
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

// -----------------------------------------------------------------
// Generic rendering

trait Renderable {
    def render: NodeSeq
}

trait Refreshable extends Renderable {
    val id = UUID.randomUUID.toString
    abstract override def render: NodeSeq =
        <div id={id}>{super.render}</div>
    
    def refresh(): JsCmd = SetHtml(id, render)
}
object Refreshable {
    class NeedRenderable(r: ()=>NodeSeq) extends Renderable {
        def render = r()
    }
    
    def apply(r: => NodeSeq) =
        new NeedRenderable(() => r) with Refreshable
}

trait Page extends StatefulSnippet with Renderable {
    def dispatch: DispatchIt = {
        case "render" => (in: NodeSeq) => render
    }
}

// -----------------------------------------------------------------
// Field rendering

trait FieldRender extends Renderable {
    def render = main
    
    def main: NodeSeq
}

trait FieldErrorRender extends FieldRender {
    def error: Option[String]
    def errorText: String

    abstract override def main = 
        <span class="input-error">
            {super.main}
            {(for (message <- error) yield <span class="error">{message}</span>) getOrElse(Nil)}
        </span>
}

// -----------------------------------------------------------------
// Form rendering

trait InnerFieldRender extends Renderable {
    def field: Renderable
    def render: NodeSeq = field.render
}

trait FormOuter extends Renderable {
    abstract override def render: NodeSeq = SHtml.ajaxForm(super.render)
}

// -----------------------------------------------------------------
// Submit rendering

trait SubmitRender extends FieldRender {
    val value: String
    def submitAjax(): JsCmd
    
    def main = SHtml.ajaxSubmit(value, submitAjax _)
}

// -----------------------------------------------------------------
// Error rendering

trait ErrorRender {
    def errorText: String
    
    def errors: NodeSeq = <span class="inputError">{errorText}</span>
}

// -----------------------------------------------------------------
// Field rendering

trait TextRender extends FieldRender {
    var text: String
    
    def main = (
           <input type="text" value={text}/>
        |> SHtml.onSubmit(text = _)
    )
}

trait AggregateRender extends FieldRender {
    def renderer: () => NodeSeq
    
    def main = renderer()
}

trait CaseRender
    extends FieldRender
    with ErrorRender
{
    val table: Map[String,Field[Any]]
    var selected: Option[String]
    val renderer: CaseChoices => NodeSeq
    
    def main = renderer(new CaseChoices(
        {
            val holder = SHtml.radio(
                table.keys toList,
                selected,
                s => selected = Some(s)
            )
            0 to table.keys.length map {holder(_)}
        }
    ))
}

class CaseChoices(
        radios: Seq[NodeSeq]
    )
{
    def _1 = radios(0)
    def _2 = radios(1)
    def _3 = radios(2)
    def _4 = radios(3)
    def _5 = radios(4)
    def _6 = radios(5)
}

trait ListRender extends FieldRender {
    val renderer: (Seq[ItemRender], NodeSeq) => NodeSeq
    def items: Seq[Field[Any] with Renderable]
    def addOne(): Unit
    def deleteOne(n: Int): Unit
    
    def main = refresh.render
    val refresh = Refreshable(renderer(
        items zip items.indices map { case (item, index) =>
            ItemRender(item.render, del(index))
        },
        add
    ))
    
    def add: NodeSeq = SHtml.ajaxSubmit("Add", { () =>
        addOne()
        refresh.refresh()
    })

    def del(n: Int): NodeSeq = SHtml.ajaxSubmit("Del", { () =>
        deleteOne(n)
        refresh.refresh()
    })
}

case class ItemRender(
        field:  NodeSeq,
        delete: NodeSeq
    )

trait CheckBoxRender extends FieldRender {
    var state: Boolean
    
    def main: NodeSeq = SHtml.checkbox(state, state = _)
}

trait DateRender extends TextRender {
    def format: NodeSeq = <span>{DateField.formatSpec}</span>
}

trait DependentListRender extends FieldRender {
    val renderer: () => NodeSeq
    
    def main: NodeSeq = renderer()
}

trait TextAreaRender extends FieldRender {
    val initText: String
    var text: String
    
    def main: NodeSeq = SHtml.onSubmit(text = _) {
        <textarea>{text}</textarea>
    }
}

