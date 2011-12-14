
// Written by: Owen Healy

package code
package snippet

import intform._

import model._
import model.schema._

class LeagueField(
        initText: String
    )
    extends TextField[League](initText)
{
    def produce() =
        League byName text map (OK(_)) getOrElse Error("No league named " + text)
}
object LeagueField {
    def apply(i: String = "") = new LeagueField(i)
}

