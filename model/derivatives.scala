
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
    securities: Seq[Security],
    exec:       DateTime,
    condition:  Condition,
    early:      Boolean
) {
    def serialize: Array[Byte] = serialization.serialize(this)
    
    def *(scale: BigDecimal): Derivative = this.copy(
        securities = securities map (_ * scale)
    )
}

object Derivative extends Loggable {
    def deserialize(bs: Array[Byte]): Derivative =
        serialization.deserialize[Derivative](bs)
}

// -------------------------------------------

sealed abstract class Security {
    def *(scale: BigDecimal): Security
}

case class SecDollar(
        amount: BigDecimal
    ) extends Security
{
    def *(scale: BigDecimal) = SecDollar(amount * scale)
}

case class SecStock(
        ticker: String,
        shares: BigDecimal
    ) extends Security
{
    def *(scale: BigDecimal) = SecStock(ticker, shares*scale)
}

case class SecDerivative(
        name:  String,
        scale: BigDecimal
    ) extends Security
{
    def *(nextScale: BigDecimal) = SecDerivative(name, scale*nextScale)
}

// -------------------------------------------

sealed abstract class Condition

case object CondAlways extends Condition

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

