
import net.liftweb.http
import http._

package object sessions {
    
    class ColonEq[A](c: SessionVar[A]) {
        def :=(a: A) = c.apply(a)
    }
    
    // Makes session variables prettier
    implicit def toColon[A](c: SessionVar[A]) =
        new ColonEq(c)
    
}

