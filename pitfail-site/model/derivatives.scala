
package code
package model

package object derivatives {

import java.sql.Timestamp
// Joda time
import org.joda.time.{DateTime}
import lib.formats._

import net.liftweb.common.Loggable

// --------------------------------------------------------------------
// The data types

case class Derivative(
    security:  Security,
    exec:      DateTime,
    condition: Condition
) {
    def serialize: Array[Byte] = serialization.serialize(this)
}

object Derivative extends Loggable {
    def deserialize(bs: Array[Byte]): Derivative =
        serialization.deserialize[Derivative](bs)
}

// -------------------------------------------

sealed abstract class Security

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

sealed abstract class Condition

case class CondAlways() extends Condition

case class CondGreater(
        a: ComparableSecurity,
        b: ComparableSecurity
    ) extends Condition

// -------------------------------------------

sealed abstract class ComparableSecurity

case class CompSecStock(
        ticker: String,
        shares: BigDecimal
    ) extends ComparableSecurity

case class CompSecDollar(
        amount: BigDecimal
    ) extends ComparableSecurity

} // package

