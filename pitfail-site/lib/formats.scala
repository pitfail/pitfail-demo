
package code
package lib

package object formats {

import model.derivatives._
import scala.math.{BigDecimal}

// -----------------------------------------------
// Money

class BigDecimalFormatted(b: BigDecimal) {
    
    def toDollarString: String =
        "$%.2f" format (b doubleValue)
}
implicit def bigDecimalFormatted(b: BigDecimal): BigDecimalFormatted
    = new BigDecimalFormatted(b)
    
// -----------------------------------------------
// Derivatives

case class FormattedSecurity(
    security: Security
) {
    def toHumanString: String = security match {
        case SecDollar(amount)      => amount toDollarString
        case SecStock(ticker)       => ticker
        case SecDerivative(name)    => "[%s]" format name
        case SecScale(sec, scale)   => "%s * (%s)" format (scale toString, sec toHumanString)
        case SecSum(children)       =>
            children map (_ toHumanString) map ("%s" format _) mkString " + "
    }
}
implicit def securityFormatted(s: Security): FormattedSecurity
    = FormattedSecurity(s)
    
case class FormattedCondition(
    condition: Condition
) {
    def toHumanString: String = "???"
}
implicit def conditionFormatted(c: Condition): FormattedCondition
    = FormattedCondition(c)
    
case class FormattedDerivative(
    deriv: Derivative
) {
    def toHumanString: String =
        "%s on %s if %" format (
            deriv.security toHumanString,
            deriv.exec,
            deriv.condition toHumanString
        )
}
implicit def derivativeFormatted(d: Derivative): FormattedDerivative
    = FormattedDerivative(d)
    
} // package

