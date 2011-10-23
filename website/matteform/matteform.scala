
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
    field: Field[A],
    formID: Option[String] = None
    )
    extends Loggable
{
    def act(result: A): Unit
    
    val errors = new ErrorRenderable("submit") {
        def renderInner = same
    }

    def processField: Option[A] = {
        field.process
    }
    
    def render(p: RefreshPoint)(in: NodeSeq): NodeSeq = {
        val rendering = (
              ("name=submit") #> { submit =>
                  val value = (submit\"@value").text
                  SHtml.ajaxSubmit(value, processAjax(p) _)
              }
            & field.render(p)
            & errors.render
        )
        
        formID match {
            case None =>
                in |> rendering |> SHtml.ajaxForm _
            case Some(formID) =>
                val attack = ("#"+formID) #> { inIn =>
                    inIn |> rendering |> SHtml.ajaxForm _
                }
                in |> attack
        }
    }
        
    def processAjax(p: RefreshPoint)(): JsCmd = {
        logger.info("Processing AJAX")
        process()
        p.refreshCommand
    }
    
    def processPost() {
        logger.info("Processing POST")
        process()
        S.redirectTo(S.uri)
    }
    
    def process() {
        field.process() match {
            case Some(a) =>
                try {
                    clear()
                    act(a)
                }
                catch { case BadInput(msg) =>
                    logger.info("Form failed due to " + msg)
                    errors.text = Some(msg)
                }
            case None =>
                logger.info("Form failed due to child error")
        }
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
    
    def renderInner(p: RefreshPoint): CssBindFunc
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
    
    def render(p: RefreshPoint) = renderInner(p) & errors.render
}

trait RefreshableSnippet extends StatefulSnippet {
    def render(p: RefreshPoint)(in: NodeSeq): NodeSeq
    
    def dispatch: DispatchIt = {
        case "render" => renderFull _
    }
    
    val uuid = UUID.randomUUID.toString
    
    def renderFull(in: NodeSeq): NodeSeq = (
           in
        |> render(RefreshPoint(in, uuid, this)) _
        |> addID
    )
        
    def addID(in: NodeSeq) = <div id={uuid}>{in}</div>
}

case class RefreshPoint(
        orig:    NodeSeq,
        uuid:    String,
        snippet: RefreshableSnippet
    )
{
    def refreshCommand: JsCmd =
        SetHtml(uuid, snippet.renderFull(orig))
}

class ErrorRenderable(name: String) {
    var text: Option[String] = None
    
    def render: CssBindFunc =
        text match {
            case None      => same
            case Some(msg) => ("#"+name+"Error *") #> msg
        }
}

