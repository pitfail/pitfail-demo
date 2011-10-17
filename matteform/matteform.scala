
package matteform

import net.liftweb.{common, http, util}
import common.{Loggable,Logger}
import util._
import scala.xml.{NodeSeq}
import java.util.UUID
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import scalaz.Scalaz._

abstract class Form[A](
    snippet: RenderableSnippet,
    field: Field[A]
    )
    extends Loggable
{
    def act(result: A): Unit
    
    val errors = new ErrorRenderable("submit") {
        def renderInner = same
    }
    
    def render(in: NodeSeq): NodeSeq = {
        val formID = UUID.randomUUID.toString
        def setID(inner: NodeSeq) = <div id={formID}>{inner}</div>
        
        val rendering = (
              "name=submit" #> { submit =>
                  val value = (submit\"@value").text
                  SHtml.ajaxSubmit(value, processAjax(formID, in) _)
              }
            & field.render
            & errors.render
        )
        
        in |> snippet.render _ |> rendering |> SHtml.ajaxForm _ |> setID _
    }
        
    def processAjax(formID: String, in: NodeSeq)(): JsCmd = {
        logger.info("Processing AJAX")
        process() & SetHtml(formID, render(in))
    }
    
    def processPost() {
        logger.info("Processing POST")
        process()
        S.redirectTo(S.uri)
    }
    
    def process(): JsCmd =
        field.process() match {
            case Some(a) =>
                try {
                    clear()
                    act(a)
                }
                catch { case BadInput(msg) =>
                    logger.info("Form failed due to " + msg)
                    errors.text = Some(msg)
                    Noop
                }
            case None =>
                logger.info("Form failed due to child errorZ")
                Noop
        }
    
    def clear() {
        field.clear()
        errors.text = None
    }
}

abstract class Field[+A](val name: String)
    extends Loggable
{
    val errors = new ErrorRenderable(name)
    
    def renderInner: CssBindFunc
    def produce(): FieldResult[A]
    def clearInner(): Unit
    
    def clear() {
        errors.text = None
        clearInner()
    }
    
    def process(): Option[A] =
        produce() match {
            case OK(a)  =>
                errors.text = None
                Some(a)
            case Error(msg) =>
                errors.text = Some(msg)
                None
            case ChildError =>
                errors.text = None
                None
        }
    
    def render = renderInner & errors.render
}
object Form {
    import http._
    import S._
    import common._
    import util._
    import util.Helpers._
    import http.js._
    import http.js.AjaxInfo
    import JE._
    import JsCmds._
    import scala.xml._
    import SHtml._

    // The default SHtml.ajaxSubmit returns true for some reason...
    def ajaxSubmit(value: String, func: () => JsCmd): Elem = {
        val funcName = "z" + Helpers.nextFuncName
        addFunctionMap(funcName, contextFuncBuilder(func))

        <input type="submit"
            name={funcName}
            value={value}
            onclick={"liftAjax.lift_uriSuffix = '"+funcName+"=_'; return true;"}
        />
    }
}

trait RenderableSnippet extends StatefulSnippet {
    def render(in: NodeSeq): NodeSeq = in
}

class ErrorRenderable(name: String) {
    var text: Option[String] = None
    
    def render: CssBindFunc =
        text match {
            case None      => same
            case Some(msg) => ("#"+name+"Error *") #> msg
        }
}

