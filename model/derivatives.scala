
package model

package object derivatives {

import java.sql.Timestamp
// Joda time
import org.joda.time.{DateTime}
import formats._
import Schema._

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
    
    def *(scale: Scale): Derivative = this.copy(
        securities = securities map (_ * scale)
    )
    
    def spotValue: Dollars = (securities map (_.spotValue))
        .foldLeft(Dollars(0))(_ + _)
}

object Derivative extends Loggable {
    def deserialize(bs: Array[Byte]): Derivative =
        serialization.deserialize[Derivative](bs)
}

// -------------------------------------------

sealed abstract class Security {
    def *(scale: Scale): Security
    
    def spotValue: Dollars
}

case class SecDollar(
        amount: Dollars
    ) extends Security
{
    def *(scale: Scale) = SecDollar(amount * scale)
    
    def spotValue = amount
}

case class SecStock(
        ticker: String,
        shares: Shares
    ) extends Security
{
    def *(scale: Scale) = SecStock(ticker, shares*scale)
    
    def spotValue = Stocks.stockPrice(ticker) * shares
}

case class SecDerivative(
        name:  String,
        scale: Scale 
    ) extends Security
{
    def *(nextScale: Scale) = SecDerivative(name, scale*nextScale)
    
    def spotValue = {
        val liab = DerivativeLiability byName name
        liab match {
            case None       => Dollars(0)
            case Some(liab) => liab.derivative.spotValue * scale
        }
    }
}

// -------------------------------------------

sealed abstract class Condition {
    def isTrue: Boolean
}

case object CondAlways extends Condition {
    def isTrue = true
}

case class CondGreater(
        a: ComparableSecurity,
        b: ComparableSecurity
    )
    extends Condition
{
    def isTrue = a.toPrice > b.toPrice
}

// -------------------------------------------

sealed abstract class ComparableSecurity {
    def toPrice: Price
}

case class CompSecStock(
        ticker: String
    ) extends ComparableSecurity
{
    def toPrice = Stocks.stockPrice(ticker)
}

case class CompSecDollar(
        amount: Price
    ) extends ComparableSecurity
{
    def toPrice = amount
}

} // package

