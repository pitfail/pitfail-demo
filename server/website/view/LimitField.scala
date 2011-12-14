
// Written by: Owen Healy

package code
package snippet

import model._
import model.schema._
import intform._

class LimitField extends Field[Option[Price]] with FieldRender {
    lazy val useField   = BooleanField()
    lazy val limitField = PriceField()
    
    def produce() = useField.process flatMap { use =>
        if (use) limitField.process map {p => OK(Some(p))}
        else Some(OK(None))
    } getOrElse ChildError
    
    def reset() { useField.reset(); limitField.reset() }
    
    def main =
        <p>{useField.main} Keep the order alive, at a limit
            of ${limitField.main & <input class="blank"/>}/sh {limitField.errors}</p>
}
object LimitField {
    def apply() = new LimitField
}

