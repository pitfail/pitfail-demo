
package code
package comet

import net.liftweb.{common, http, util}
import common.{Loggable}
import http._

import net.liftweb.actor
import actor._

case object Refresh
    
trait Refreshable extends CometActor with CometListener with Loggable
{
    override def lowPriority: PartialFunction[Any, Unit] = {
        case Refresh =>
            logger.info("Receiving a refresh!")
            refresh()
            reRender()
    }
    
    def refresh() { }
}

trait RefreshHub extends LiftActor
    with ListenerManager
    with Loggable
{
    def createUpdate = Refresh
    
    override def lowPriority = {
        case Refresh =>
            logger.info("Receiving a refresh!")
            updateListeners()
    }
    
    def apply() { this ! Refresh }
}
object RefreshHub {
    implicit def toThunk(r: RefreshHub): () => Unit = () => r()
}

