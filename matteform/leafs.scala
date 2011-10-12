
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

// ------------------------------------------------------------
// StringField

class StringField(
    name: String,
    val initText: String
    )
    extends Field[String](name)
    with TextField
{
    def produce() = Right(text)
}
object StringField {
    def apply(n: String, i: String): Field[String]
        = new StringField(n, i)
}

// ------------------------------------------------------------
// NumberField

class NumberField(
    name: String,
    val initText: String
    )
    extends Field[BigDecimal](name)
    with TextField
{
    def produce() =
        try {
            Right(BigDecimal(text))
        }
        catch {
            case e: NumberFormatException => Left("Should be a number")
        }
}
object NumberField {
    def apply(n: String, i: String): Field[BigDecimal]
        = new NumberField(n, i)
}

// ------------------------------------------------------------
// HiddenField

class HiddenField(
    name: String
    )
    extends Field[String](name)
{
    var text: Option[String] = None
    
    def renderInner = ("#"+name) #> SHtml.onSubmit(t => text = Some(t))
    
    def produce() = text match {
        case Some(text) => Right(text)
        case _ =>
            throw new IllegalStateException(name+" should have been set")
    }
    
    def clearInner() { }
}
object HiddenField {
    def apply(name: String): Field[String]
        = new HiddenField(name)
}

// ------------------------------------------------------------
// ConstField

class ConstField[+A](
    val it: A
    )
    extends Field[A]("noname")
{
    def renderInner = same
    def produce() = Right(it)
    def clearInner() { }
}
object ConstField {
    def apply[A](it: A): Field[A] = new ConstField(it)
}

// ------------------------------------------------------------
// AttrField

class AttrField(
    name: String
    )
    extends Field[String]("noname")
    with Loggable
{
    var text: Option[String] = None
    
    def renderInner =
        ("name="+name) #> { p =>
            val attr = {
                try {
                    (p \ "@value").text
                }
                catch { case _: Any =>
                    throw new IllegalStateException(
                          "Hidden field "
                        + p + " needs value= attribute"
                    )
                }
            }
            SHtml.onSubmitUnit(() => {
                logger.info(attr + " got submitted")
                text = Some(attr)
            })(p)
        }
    
    def produce() = text match {
        case Some(text) => Right(text)
        case _ =>
            throw new IllegalStateException(name+" should have been set")
    }
    
    def clearInner() { }
    
    override def toString: String = "(attr " + name + ")"
}
object AttrField {
    def apply(name: String): Field[String]
        = new AttrField(name)
}

// -------------------------------------------------------------------

trait TextField
{
    val name: String
    val initText: String
    
    var text: String = initText
    
    def renderInner =
        ("name="+name) #> (
              "input [value]" #> text
            & "input"         #> SHtml.onSubmit(text = _)
        )
    
    def clearInner() { text = initText }
}

