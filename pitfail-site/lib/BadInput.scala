
package code
package lib

case class BadInput(
    msg: String
)
    extends Exception(msg)

