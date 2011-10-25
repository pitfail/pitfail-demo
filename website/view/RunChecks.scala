
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import intform._

class RunChecks extends Page with Loggable
{
    def render = FormSubmit.rendered("Run Checks") {
        import comet._
        
        logger.info("Running all background checks...")
        model.Schema.checkForExercise()
        
        Portfolio ! Refresh
        News ! Refresh
    }
}

