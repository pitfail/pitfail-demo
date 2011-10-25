
package code
package control

import net.liftweb.{util, common}
import util._
import util.Helpers._
import common.Loggable

object DerivativeChecker extends Loggable {
    
    def run(): Unit = periodically(30 minutes) {
        logger.info("Running the derivative checker...")
        model.Schema.checkForExercise()
    }
}

