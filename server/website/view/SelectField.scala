
// Written by: Owen Healy

package code
package snippet

import net.liftweb.http._
import net.liftweb.common._
import scala.xml.{NodeSeq}
import java.util.UUID
import intform._

class SelectField[+A](
    val choices: Vector[(A,String)],
    initState: Int
)
    extends Field[A]
    with SelectRender
{
    val names = choices map (_._2)
    var chosen: Int = initState
    
    def reset() { chosen = initState }
    def produce() = OK(choices(chosen)._1)
}
object SelectField {
    def apply[A](
        c: Seq[(A,String)],
        a: A
    ) = {
        val v = Vector(c:_*)
        val i = v map (_._1) indexOf a
        assert(i != -1,
            "That's not a valid option %s in %s"
            format (a, c)
        )
        
        new SelectField(v, i)
    }
}

trait SelectRender extends FieldRender with Loggable {
    def names: Vector[String]
    var chosen: Int
    
    def main = {
        val ids = names map (_ => UUID.randomUUID.toString)
        val chosenID = ids(chosen)
        
        SHtml.select(
            ids zip names,
            Some(chosenID),
            n => chosen = {
                val i = ids.indexOf(n)
                if (i == -1) 0 else i
            }
        )
    }
}

