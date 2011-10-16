
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
        field.process() match {
            case Some(a) =>
                try {
                    act(a)
                    clear()
                }
                catch { case BadInput(msg) =>
                    logger.info("Form failed due to " + msg)
                    errors.text = Some(msg)
                }
            case None =>
                logger.info("Form failed due to child errorZ")
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

class ErrorRenderable(name: String) {
    var text: Option[String] = None
    
    def render: CssBindFunc =
        text match {
            case None      => same
            case Some(msg) => ("#"+name+"Error *") #> msg
        }
}

