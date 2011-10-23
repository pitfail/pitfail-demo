
package code
package snippet

import model.Schema.{
    User
}

import intform._

class UserField(
        initText: String
    )
    extends TextField[User](initText)
{
    def produce() =
        model.Schema.byUsername(text) match {
            case None => Error("No user named " + text)
            case Some(user) => OK(user)
        }
}
object UserField {
    def apply(i: String = "") = new UserField(i)
}

