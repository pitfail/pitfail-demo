
package code
package snippet

import model.Schema.Price
import model.derivatives._
import intform._

object ConditionField {
    type CS = ComparableSecurity
    
    def apply() = {
        lazy val field = AggregateField(
            make _,
            (first, second),
            <div>
                {first.main} {first.errors} &lt; {second.main} {second.errors}
            </div>
        )
        lazy val first  = CompSecField()
        lazy val second = CompSecField()
        
        field
    }
    
    def make(a: CS, b: CS) = CondGreater(b, a)
}

class CompSecField
    extends TextField[ComparableSecurity]("")
    with ErrorRender
{
    def produce() =
        try {
            OK(CompSecDollar(Price(text)))
        }
        catch {
            case _: NumberFormatException => OK(CompSecStock(text))
        }
}
object CompSecField {
    def apply() = new CompSecField
}

