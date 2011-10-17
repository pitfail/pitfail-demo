
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import matteform._
import scalaz.Scalaz._

class TestForm extends RenderableSnippet with Loggable {
    
    var count: Int = 0
    
    def dispatch = {
        case "render" => form.render _
    }
    
    object form extends Form[Unit](this,
        ConstField(())
    )
    {
        def act(u: Unit) {
            count += 1
        }
    }
    
    override def render(in: NodeSeq): NodeSeq =
        in |> renderCount
    
    def renderCount = "#count *" #> count
    
    def processAjax(in: NodeSeq)(): JsCmd = {
        count += 1
        SetHtml("testForm", render(in))
    }
}

