
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
import scala.math._
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
            count: BigDecimal
        )
    
    lazy val submit = Submit(form, "Send") { order =>
        logger.info(order)
        Noop
    }
    
    lazy val form: Form[Order] = Form(Order,
        (
            symField,
            countField
        ),
        <table>
            <tr>
                <td>Symbol:</td>
                <td>{symField.main}</td>
                <td>{symField.errors}</td>
            </tr>
            <tr>
                <td>Count:</td>
                <td>{countField.main}</td>
                <td>{countField.errors}</td>
            </tr>
            <tr><td>{submit.main}</td></tr>
            <tr><td>{submit.errors}</td></tr>
        </table>
    )
    lazy val symField = StringField()
    lazy val countField = NumberField()
}

