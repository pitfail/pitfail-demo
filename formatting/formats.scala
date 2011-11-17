
package object formats {
//

import scala.math.{BigDecimal}
import org.joda.time.{DateTime}
import net.liftweb.common.Logger
import java.sql.Timestamp

import model._
import model.schema._

// ------------------------------------------------------------------
// Errors

def standardMessage(error: Any) = {
    error match {
        case NegativeVolume               => "You must buy more than $0.00"
        case NotEnoughCash(have, need)    => "You need %s you have %s" format (have.$, need.$)
        case DontOwnStock(ticker)         => "You don't own %s" format ticker
        case NotEnoughShares(have, need)  => "You have %s shares you need %s" format (have.###(), need.###())
        case OfferExpired                 => "The offer has already expired"
        case NotExecutable                => "You can't exercise this derivative early"
        
        case e =>
            new Logger { error("An internal error occurred: " + e) }
            "An internal error occurred. Check the logs."
    }
}

// ------------------------------------------------------------------
// Time

case class DateTimeFormatted(d: DateTime) {
    // TODO: This should adapt better
    def toNearbyString: String = d toString "M/d"
}
implicit def dateTimeFormatted(d: DateTime) = DateTimeFormatted(d)
implicit def timestampFormatted(t: Timestamp) = DateTimeFormatted(new DateTime(t.getTime))

// ------------------------------------------------------------------
// Money

case class BigDecimalFormatted(b: BigDecimal) {
    def $: String = "$%.2f" format (b doubleValue)
    def %(): String = "%.0f%%" format (b.doubleValue * 100)
    def ###(): String = "%.0f" format (b doubleValue)

}
implicit def bigDecimalFormatted(b: BigDecimal): BigDecimalFormatted = BigDecimalFormatted(b)

case class BigDecimalOptionFormatted(b: Option[BigDecimal]) {
    def $: String = {
        (b map (_.$)) getOrElse "n/a" 
    }

    def %(): String = {
        (b map (_.toString + "%")) getOrElse "n/a"
    }
}
implicit def bigDecimalOptionFormatted(b: Option[BigDecimal]): BigDecimalOptionFormatted = BigDecimalOptionFormatted(b)
    
// ------------------------------------------------------------------
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
        case SecStock(ticker, shares)   => "%s shrs %s" format (shares.###(), ticker)
        case SecDerivative(name, scale) => "[%s %s]" format (scale.%(), name)
    }
}
implicit def securityFormatted(s: Security): FormattedSecurity
    = FormattedSecurity(s)
    
case class FormattedCondition(
    condition: Condition
) {
    def toHumanString = condition match {
        case CondAlways        => "-"
        case CondGreater(a, b) => "%s > %s" format (a toHumanString, b toHumanString)
    }
}
implicit def conditionFormatted(c: Condition): FormattedCondition
    = FormattedCondition(c)
    
case class CompSecFormatted(c: ComparableSecurity) {
    def toHumanString = c match {
        case CompSecStock(ticker)  => ticker
        case CompSecDollar(amount) => amount.$
    }
}
implicit def compSecFormatted(c: ComparableSecurity): CompSecFormatted
    = CompSecFormatted(c)
    
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
    
//
}

