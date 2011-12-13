
// Written by: Michael Koval

import scala.math.{BigDecimal}
import java.math.{MathContext,RoundingMode}

import org.squeryl
import squeryl.customtypes.{BigDecimalField,IntField}
import net.liftweb.common.Logger
import model.spser._
import net.liftweb.json._

package object model {
//

type Key = String
def nextID: Key = java.util.UUID.randomUUID.toString.substring(0, 5)

implicit def bigDecimalOps(b: BigDecimal) = new {
    def round(decimals: Int, mode: RoundingMode): BigDecimal = {
        val precision = b.precision - b.scale + decimals
        if (precision > 0) {
            val context = new MathContext(precision, mode)
            val out = b.round(context)
            out
        }
        else BigDecimal("0")
    }

    def floor: BigDecimal =
        round(0, RoundingMode.FLOOR)
    
    def evenCents: BigDecimal =
        round(2, RoundingMode.FLOOR)
}

implicit def whyDidTheyMakeTheSameMistakeAsHaskell[A]
        (s: TraversableOnce[A])(implicit add: Add[A]) = new
{
    def summ: A = s.foldLeft(add.zero)(add.add _)
}

implicit val addDollars: Add[Dollars] = new Add[Dollars] {
    val zero = Dollars(0)
    def add(d1: Dollars, d2: Dollars) = d1 + d2
}

implicit val addShares: Add[Shares] = new Add[Shares] {
    val zero = Shares(0)
    def add(s1: Shares, s2: Shares) = s1 + s2
}

implicit val addPrice: Add[Price] = new Add[Price] {
    val zero = Price(0)
    def add(p1: Price, p2: Price) = p1 + p2
}
    
implicit def sqlDollars: SQLType[Dollars] = new SQLType[Dollars] {
    type CT = String
    val typeName = "VARCHAR"
    def encode(d: Dollars) = d.dollars.toString
    def decode(b: String) = Dollars(b)
}

implicit def sqlShares: SQLType[Shares] = new SQLType[Shares] {
    type CT = String
    val typeName = "VARCHAR"
    def encode(d: Shares) = d.shares.toString
    def decode(b: String) = Shares(b)
}

implicit def sqlPrice: SQLType[Price] = new SQLType[Price] {
    type CT = String
    val typeName = "VARCHAR"
    def encode(d: Price) = d.price.toString
    def decode(b: String) = Price(b)
}

implicit def sqlScale: SQLType[Scale] = new SQLType[Scale] {
    type CT = String
    val typeName = "VARCHAR"
    def encode(d: Scale) = d.scale.toString
    def decode(b: String) = Scale(b)
}

//
}

package model {
//

trait Add[A] {
    val zero: A
    def add(a1: A, a2: A): A
}

// ref_868
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
    def /(shares: Shares) = Price {
        try
            dollars/shares.shares
        catch {
            case e: Exception =>
                new Logger { info("Can't dived %s by %s" format (dollars, shares.shares)) }
                throw e
        }
    }

    def compare(other: Dollars) = dollars.compare(other.dollars)

    def $:   String = 
        if (dollars < 0) "-$%.2f" format (-(dollars doubleValue))
        else "$%.2f" format (dollars doubleValue)
        
    def $short: String =
        if      (dollars >= 1000000) "$%.0fM" format ((dollars doubleValue)/1000000)
        else if (dollars >= 1000)    "$%.0fk" format ((dollars doubleValue)/1000)
        else                                  "$%.0f"  format ((dollars doubleValue))
        
    def no$: String = "%.2f" format (dollars doubleValue)
    
    def double = dollars.doubleValue
    def isNegative = this < Dollars(0)
}

object Dollars {
    def apply(str: String): Dollars = Dollars(BigDecimal(str))
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

    override def toString(): String = ###() + (if (shares != 1) " shares" else " share")
    
    def double = shares.doubleValue
    def isNegative = this < Shares(0)
}
object Shares {
    def apply(str: String): Shares = Shares(BigDecimal(str))
}

case class Price(price: BigDecimal) extends Ordered[Price] {
    def this(str: String) = this(BigDecimal(str))

    def +(other: Price)   = Price(price + other.price)
    def -(other: Price)   = Price(price - other.price)
    def *(shares: Shares) = Dollars(shares.shares * price)
    def *(scale: Scale)   = Price(scale.scale * price)
    def unary_- = copy(price = -price)

    def compare(other: Price) = price.compare(other.price)

    def $:   String = 
        if (price < 0) "-$%.2f" format (-(price doubleValue))
        else "$%.2f" format (price doubleValue)
        
    def double = price.doubleValue
    def isNegative = this < Price(0)
}
object Price {
    def apply(str: String): Price = Price(BigDecimal(str))
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
    def isNegative = this < Scale(0)
}
object Scale {
    def apply(str: String): Scale = Scale(BigDecimal(str))
}

//
}

