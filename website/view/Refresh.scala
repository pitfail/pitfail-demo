
package code
package comet

import net.liftweb.{common, http, util}
import common.{Loggable}
import http._

import net.liftweb.actor
import actor._

trait Refreshable extends CometActor with CometListener with Loggable
{
    override def lowPriority: PartialFunction[Any, Unit] = {
        case model.Refresh =>
            refresh()
            reRender()
    }
    
    def refresh() { }
}

