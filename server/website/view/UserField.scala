
// Written by: Owen Healy

package code
package snippet

import intform._

import model._
import model.schema._

class UserField(
        initText: String
    )
    extends TextField[User](initText)
{
    def produce() =
        try readDB {
            OK(User byName text)
        }
        catch { case NoSuchUser =>
            Error("No user named " + text)
        }
}
object UserField {
    def apply(i: String = "") = new UserField(i)
}

