
package code
package control

import net.liftweb.{util, actor}
import actor._
import util._
import util.Helpers._

object periodically {
    def apply(delay: TimeSpan)(command: =>Unit) {
        val actor = new LiftActor {
            def messageHandler = {
                case _ =>
                    Schedule.schedule(this, 'yo, delay)
                    command
            }
        }
        Schedule.schedule(actor, 'yo, delay)
    }
}

