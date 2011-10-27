
import scala.math.{BigDecimal}
import java.math.{MathContext,RoundingMode}

import org.squeryl
import squeryl.customtypes.{BigDecimalField,IntField}
import net.liftweb.common.Logger

package object model {
//

implicit def bigDecimalOps(b: BigDecimal) = new {
    def round(decimals: Int, mode: RoundingMode): BigDecimal = {
        val precision = b.precision - b.scale + decimals
        if (precision > 0) {
            val context = new MathContext(precision, mode)
            val out = b.round(context)
            new Logger {
                info("Rounding!!!!")
                info(out)
                info("%s" format out.precision)
            }
            out
        }
        else BigDecimal("0")
    }

    def floor: BigDecimal =
        round(0, RoundingMode.FLOOR)
    
    def evenCents: BigDecimal =
        round(2, RoundingMode.FLOOR)
}

//
}

package model {
//

case class Dollars(dollars: BigDecimal)
    extends Ordered[Dollars]
{
    def this(str: String) = this(BigDecimal(str))
    
    def +(other: Dollars) = Dollars(dollars + other.dollars)
    def -(other: Dollars) = Dollars(dollars - other.dollars)
    def *(scale: Scale)   = Dollars(scale.scale * dollars)
    def unary_- = copy(dollars = -dollars)
    
    // This makes me suspicious.
    // def /(shares: Shares) = Price(dollars / shares.shares)
    
    def ~/~(price: Price) = Shares(dollars / price.price)
    def /-/(price: Price) = Shares((dollars / price.price).floor)

    def compare(other: Dollars) = dollars.compare(other.dollars)

    def $:   String = 
        if (dollars < 0) "-$%.2f" format (-(dollars doubleValue))
        else "$%.2f" format (dollars doubleValue)
        
    def no$: String = "%.2f" format (dollars doubleValue)
}
object Dollars {
    def apply(str: String): Dollars = Dollars(BigDecimal(str))
    
    implicit def toField(d: Dollars) = DollarsField(d.dollars)
}

case class DollarsField(dollars: BigDecimal)
    extends IntField((dollars*100).intValue)
{
    def this(cents: Int) = this(BigDecimal(cents)/100)
}
object DollarsField {
    implicit def fromField(d: DollarsField) = Dollars(d.dollars)
}

case class Shares(shares: BigDecimal) extends Ordered[Shares] {
    def this(str: String) = this(BigDecimal(str))
    
    def +(other: Shares) = Shares(shares + other.shares)
    def -(other: Shares) = Shares(shares - other.shares)
    def *(price: Price)  = Dollars(price.price * shares)
    def *(scale: Scale)   = Shares(scale.scale * shares)
    def unary_- = copy(shares = -shares)
    
    def compare(other: Shares) = shares.compare(other.shares)

    def ###(): String = "%.0f" format (shares doubleValue)
}
object Shares {
    def apply(str: String): Shares = Shares(BigDecimal(str))
    
    implicit def toField(s: Shares) = SharesField(s.shares)
}

case class SharesField(shares: BigDecimal)
    extends BigDecimalField(shares)
{
}
object SharesField {
    implicit def fromField(s: SharesField) = Shares(s.shares)
}

case class Price(price: BigDecimal) extends Ordered[Price] {
    def this(str: String) = this(BigDecimal(str))

    def +(other: Price)   = Price(price + other.price)
    def -(other: Price)   = Price(price - other.price)
    def *(shares: Shares) = Dollars(shares.shares * price)
    def *(scale: Scale)   = Price(scale.scale * price)

    def compare(other: Price) = price.compare(other.price)

    def $:   String = 
        if (price < 0) "-$%.2f" format (-(price doubleValue))
        else "$%.2f" format (price doubleValue)
}
object Price {
    def apply(str: String): Price = Price(BigDecimal(str))

    implicit def toField(p: Price) = PriceField(p.price)
}

case class PriceField(price: BigDecimal) extends BigDecimalField(price) {
}
object PriceField {
    implicit def fromField(p: PriceField) = Price(p.price)
}

case class Scale(scale: BigDecimal) extends Ordered[Scale] {
    def +(other: Scale)              = Scale(scale + other.scale)
    def -(other: Scale)              = Scale(scale - other.scale)
    def *(dollars: Dollars): Dollars = Dollars(scale * dollars.dollars)
    def *(price: Price): Price       = Price(scale * price.price)
    def *(shares: Shares): Shares    = Shares(scale * shares.shares)
    def *(other: Scale): Scale       = Scale(scale * other.scale)
    def unary_- = copy(scale = -scale)

    def compare(other: Scale) = scale.compare(other.scale)

    def %(): String = "%.0f%%" format ((scale doubleValue) * 100)
}
object Scale {
    def apply(str: String): Scale = Scale(BigDecimal(str))
    
    implicit def toField(s: Scale) = ScaleField(s.scale)
}

case class ScaleField(scale: BigDecimal) extends BigDecimalField(scale) {
}
object ScaleField {
    implicit def fromField(s: ScaleField) = Scale(s.scale)
}

//
}

