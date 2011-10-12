
package code.comet

import net.liftweb._
import common._
import http._
import util._
import Helpers._

class TheBox extends CometActor
    with CometListener
    with Loggable
{
    private var boxContents: String = ""
    
    def registerWith = TheBoxServer
    
    override def lowPriority = {
        case s: String => {
            boxContents = s
            reRender()
            logger.info("Got " + s)
        }
    }
    
    def render = {
        logger.info("Rendering")
        "#thebox_content" #> boxContents
    }
}

