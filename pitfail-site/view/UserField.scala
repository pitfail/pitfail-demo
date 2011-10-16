
package code
package view

import model.Schema.{
    User
}

import matteform._

class UserField(
        // Field name, not the user name
        name: String,
        val initText: String
    )
    extends Field[User](name) with TextField
{
    def produce() =
        model.Schema.byUsername(text) match {
            case None =>
                Error("User `"+text+"` does not exist")
                
            case Some(user) => OK(user)
        }
}
object UserField {
    def apply(n: String, i: String) = new UserField(n, i)
}

