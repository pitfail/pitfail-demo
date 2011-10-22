
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
    def errors: NodeSeq
}

// -----------------------------------------------------------------
// Form rendering

trait InnerFieldRender extends Renderable {
    def field: Field[Any]
    
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

trait ErrorRender extends FieldRender {
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

trait CaseRender extends FieldRender {
    val renderer: List[ChoiceRender] => NodeSeq
    
    var selected: Option[String]
    val table: Map[String, Field[Any]]
    
    def main = {
        val radios = SHtml.radio(
            table.keys toList,
            selected,
            n => selected = Some(n)
        )
        
        renderer(table map { case (name, field) =>
            new ChoiceRender(field, radios(name))
        } toList)
    }
}

