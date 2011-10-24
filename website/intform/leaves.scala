
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

// -----------------------------------------------------------------------
// TextField

abstract class TextField[+A](initText: String)
    extends Field[A]
    with TextRender
    with ErrorRender
{
    var text = initText
    
    def reset() { text = initText }
}

// -----------------------------------------------------------------------
// StringField

class StringField(initText: String)
    extends TextField[String](initText)
{
    def produce() = OK(text)
}
object StringField {
    def apply(i: String = "") = new StringField(i)
}

// -----------------------------------------------------------------------
// NumberField

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

// --------------------------------------------------------------
// IntField

class IntField(initText: String)
    extends TextField[Int](initText)
{
    def produce() =
        try {
            OK(Integer.parseInt(text))
        }
        catch { case _: NumberFormatException =>
            Error("Should be an integer")
        }
}
object IntField {
    def apply(i: String = "") = new IntField(i)
}

// -----------------------------------------------------------------------
// ConstField

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

// -----------------------------------------------------------------------
// BooleanField

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

// -----------------------------------------------------------------------
// DateField

import org.joda.time._
import org.joda.time.format._

class DateField(
        initText: String
    )
    extends TextField[DateTime](initText)
    with DateRender
{
    def produce() =
        try {
            var date = DateField.format parseDateTime text
            val now = new DateTime
            date = date.withYear(now.year.get)
            if (date isBefore now) date = date.plusYears(1)
                
            OK(date)
        }
        catch {
            case _: IllegalArgumentException => Error(DateField.formatSpec)
        }
}
object DateField {
    def apply(initText: String = "") = new DateField(initText)
    
    val formatSpec = "MM/dd"
    val format = DateTimeFormat forPattern formatSpec
}

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}

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

