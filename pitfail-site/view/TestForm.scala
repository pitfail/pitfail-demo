
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

import intform._
import scalaz.Scalaz._
import up._
import HList._
import KList._
import ~>._

class TestForm extends Page with Loggable {
    
    def render = form.render
    
    case class Order(
            sym:   String,
            count: String
        )
    
    lazy val submit = Submit(form, "Send") { order =>
        logger.info(order)
        Noop
    }
    
    lazy val form: Form[Order] = Form(Order, (
            -StringField(),
            -StringField(),
            -submit
        ))(F( (sym, count, sub) =>
            <table>
                <tr>
                    <td>Symbol:</td>
                    <td>{sym.main}</td>
                    <td>{sym.errors}</td>
                </tr>
                <tr>
                    <td>Count:</td>
                    <td>{count.main}</td>
                    <td>{count.errors}</td>
                </tr>
                <tr><td>{sub.main}</td></tr>
                <tr><td>{sub.errors}</td></tr>
            </table>
        ))
}

