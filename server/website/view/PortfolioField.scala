
// Written by: Owen Healy

package code
package snippet

import intform._

import model._
import model.schema._

class PortfolioField(
        initText: String
    )
    extends TextField[Portfolio](initText)
{
    def produce() =
        try readDB {
            OK(Portfolio byName text)
        }
        catch { case NoSuchPortfolio =>
            Error("No portfolio named " + text)
        }
}
object PortfolioField {
    def apply(i: String = "") = new PortfolioField(i)
}

