
package code.lib

import net.liftweb.{common, http, openid, util}
import http._

object Sugar {
    
    def untilOK[A](gen: => A, ok: A => Boolean): A = {
        var a: A = gen
        while (! ok(a)) a = gen
        a
    }
    
    class SessionVarPlus[T](val svar: SessionVar[T]) {
        def :=(t: T) { svar(t) }
    }
    implicit def sessionVarPlus[T](svar: SessionVar[T])
        = new SessionVarPlus(svar)
}

