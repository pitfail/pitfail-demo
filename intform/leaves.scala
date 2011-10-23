
package intform

import net.liftweb.{common, http, util}
import common.Loggable
import util._
import scala.xml._
import http._
import Helpers._
import scalaz.Scalaz._
import scala.math._

import js._
import JsCmds._
import JE._

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}

abstract class TextField[+A](initText: String)
    extends Field[A]
    with TextRender
    with ErrorRender
{
    var text = initText
    
    def reset() { text = initText }
}

class StringField(initText: String)
    extends TextField[String](initText)
{
    def produce() = OK(text)
}
object StringField {
    def apply(i: String = "") = new StringField(i)
}

class NumberField(initText: String)
    extends TextField[BigDecimal](initText)
{
    def produce() = 
        try {
            OK(BigDecimal(text))
        }
        catch { case _: NumberFormatException =>
            Error("Should be a number")
        }
}
object NumberField {
    def apply(i: String = "") = new NumberField(i)
}

class ConstField[+A](
        result: A
    )
    extends Field[A]
{
    def produce() = OK(result)
    def reset() { }
}
object ConstField {
    def apply[A](r: A) = new ConstField(r)
}

class BooleanField(
        initState: Boolean
    )
    extends Field[Boolean]
    with CheckBoxRender
{
    var state: Boolean = initState
    
    def produce() = OK(state)
    def reset() { state = initState }
}
object BooleanField {
    def apply(i: Boolean = false) = new BooleanField(i)
}

class DateTimeField(initial: DateTime, formatter: DateTimeFormatter)
    extends TextField[DateTime](formatter.print(initial))
{
    def produce() = 
        try {
            OK(formatter.parseDateTime(text))
        }
        catch {
            // TODO: Handle DateTime exceptions.
            case _: NumberFormatException =>
                Error("Should be a number")
        }
}
object DateTimeField {
    def apply(initial: DateTime, formatter: DateTimeFormatter) =
        new DateTimeField(initial, formatter)
}
