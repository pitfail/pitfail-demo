
package object errors {
//

import net.liftweb.common._
import model._

class BoxOps[A](b: Box[A]) {
    def withMessage(m: String => A): A = b match {
        case Empty              => m("Unknown error")
        case Failure(msg, _, _) => m(msg)
        case Full(a)            => a
    }
}

implicit def boxOps[A](b: Box[A]): BoxOps[A] = new BoxOps(b)

// The idea of this trait is that we now that a BadUser error
// is not *our* fault; ie, PitFail isn't broken
trait BadUser extends RuntimeException

//
}

