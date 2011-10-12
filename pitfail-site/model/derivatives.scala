
package code
package model

package object derivatives {

import java.sql.Timestamp
import lib.formats._

import net.liftweb.json.{DefaultFormats,parse,render,compact}
import net.liftweb.json.Extraction
    import Extraction.{extract, decompose}

// --------------------------------------------------------------------
// The data types

case class Derivative(
    security:  Security,
    exec:      Timestamp,
    condition: Condition
) {
    import Derivative._
    
    def toJSON: String = compact(render(decompose(this)))
}

object Derivative {
    implicit val formats = DefaultFormats
    def fromJSON(json: String) = extract[Derivative](parse(json))
}

// -------------------------------------------

abstract class Security

case class SecDollar(
        amount: BigDecimal
    ) extends Security

case class SecStock(
        ticker: String
    ) extends Security

case class SecDerivative(
        name: String
    ) extends Security

case class SecScale(
        security: Security,
        scale:    BigDecimal
    ) extends Security

case class SecSum(
        securities: Seq[Security]
    ) extends Security

// -------------------------------------------

abstract class Condition

case object CondAlways extends Condition

case class CondGreater(
        a: ComparableSecurity,
        b: ComparableSecurity
    ) extends Condition

// -------------------------------------------

abstract class ComparableSecurity

case class CompSecStock(
        ticker: String,
        shares: BigDecimal
    ) extends ComparableSecurity

case class CompSecDollar(
        amount: BigDecimal
    ) extends ComparableSecurity

} // package

