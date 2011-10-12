
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

abstract class Form[A](
    refresh: () => Unit,
    field: Field[A]
    )
    extends Loggable
{
    def act(result: A): Unit
    
    val errors = new ErrorRenderable("submit") {
        def renderInner = same
    }
    
    def render(in: NodeSeq): NodeSeq = {
        logger.info("Rendiring form............")
        
        SHtml.ajaxForm(
            (
                  "name=submit" #> { submit =>
                      val value = (submit\"@value").text
                      SHtml.ajaxSubmit(value, processAjax _)
                  }
                & field.render
                & errors.render
            )(in)
        )
    }
        
    def processAjax(): JsCmd = {
        logger.info("Processing AJAX")
        process()
        refresh()
        Noop
    }
    
    def processPost() {
        logger.info("Processing POST")
        process()
        S.redirectTo(S.uri)
    }
    
    def process() {
        try {
            act(field.process())
            clear()
        }
        catch {
            case BadInput(msg) =>
                logger.info("Form failed due to " + msg)
                errors.text = Some(msg)
                
            case ChildError =>
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
    
    def renderInner: CssBindFunc
    def produce(): Either[String,A]
    def clearInner(): Unit
    
    def clear() {
        errors.text = None
        clearInner()
    }
    
    def process(): A =
        produce() match {
            case Right(a)  =>
                errors.text = None
                a
            case Left(msg) =>
                errors.text = Some(msg)
                throw ChildError
        }
    
    def render = renderInner & errors.render
}

class ErrorRenderable(name: String) {
    var text: Option[String] = None
    
    def render: CssBindFunc =
        text match {
            case None      => same
            case Some(msg) => ("#"+name+"Error *") #> msg
        }
}

