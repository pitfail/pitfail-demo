
package code.comet

import net.liftweb._
import http._
import actor._

object TheBoxServer extends LiftActor with ListenerManager {
    // Box state
    private var boxContents: String = "This is the contents of the box.";
    
    // To send to listeners (whatever that means)
    def createUpdate = boxContents
    
    override def lowPriority = {
        case s: String => updateContents(s)
    }
    
    private def updateContents(s: String) {
        boxContents = s
        updateListeners()
    }
}

