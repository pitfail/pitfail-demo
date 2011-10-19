
package code
package lib

package object formats {

import model.derivatives._
import scala.math.{BigDecimal}
import org.joda.time.{DateTime}

// -----------------------------------------------
// Time

case class DateTimeFormatted(d: DateTime) {
    // TODO: This should adapt better
    def toNearbyString: String = d toString "M/d"
}
implicit def dateTimeFormatted(d: DateTime) = DateTimeFormatted(d)

// -----------------------------------------------
// Money

case class BigDecimalFormatted(b: BigDecimal) {
    def $: String = "$%.2f" format (b doubleValue)
    def %(): String = "%.0f%%" format (b.doubleValue * 100)
    def ###(): String = "%.0f" format (b doubleValue)
}
implicit def bigDecimalFormatted(b: BigDecimal) = BigDecimalFormatted(b)
    
// -----------------------------------------------
// Derivatives

case class FormattedSecurities(
    securities: Seq[Security]
) {
    def toHumanString: String = securities map (_.toHumanString) mkString " + "
}
implicit def securitiesFormatted(ss: Seq[Security]): FormattedSecurities
    = FormattedSecurities(ss)

case class FormattedSecurity(
    security: Security
) {
    def toHumanString: String = security match {
        case SecDollar(amount)          => amount.$
        case SecStock(ticker, shares)   => "%s shares of %s" format (shares.###(), ticker)
        case SecDerivative(name, scale) => "[%s %s]" format (scale.%(), name)
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
        "%s on %s if %s" format (
            deriv.securities toHumanString,
            deriv.exec toNearbyString,
            deriv.condition toHumanString
        )
}
implicit def derivativeFormatted(d: Derivative): FormattedDerivative
    = FormattedDerivative(d)
    
} // package

