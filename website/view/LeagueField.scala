
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
    def produce() = {
        val l = readDB (League byName text)
        l match {
            case Some(ll) => OK(ll)
            case None     => Error("No league named \"" + text + "\"")
        }
    }
}

object LeagueField {
    def apply(i: String = "") = new LeagueField(i)
}
