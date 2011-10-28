
package code
package snippet

import net.liftweb.{common, http, util}
import common.Loggable
import util._
import scala.xml._
import Helpers._
import java.util.UUID

import up._
import HList._
import KList._
import ~>._
import scalaz.Scalaz._
import scala.math._

import intform._
import model._
import model.Schema._

class DollarsField(initText: String)
    extends TextField[Dollars](initText)
{
    def produce() = 
        try {
            OK(Dollars(text))
        }
        catch { case _: NumberFormatException =>
            Error("Must be a dollar amount")
        }
}
object DollarsField {
    def apply(i: String = "") = new DollarsField(i)
}

class PriceField(initText: String)
    extends TextField[Price](initText)
{
    def produce() = 
        try {
            OK(Price(text))
        }
        catch { case _: NumberFormatException =>
            Error("Should be a price in dollars")
        }
}
object PriceField {
    def apply(i: String = "") = new PriceField(i)
}

class SharesField(initText: String)
    extends TextField[Shares](initText)
{
    def produce() = 
        try {
            OK(Shares(text))
        }
        catch { case _: NumberFormatException =>
            Error("Should be a price in dollars")
        }
}
object SharesField {
    def apply(i: String = "") = new SharesField(i)
}
