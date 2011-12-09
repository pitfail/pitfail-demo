
package code
package control

import net.liftweb.{util, common}
import util._
import util.Helpers._
import common.Loggable

import model._ 
import model.schema._

object Checker extends Loggable {
    
    def run(): Unit = periodically(30 minutes) {
        runChecks()
    }
    
    def runChecks() {
        logger.info("Running the derivative checker...")
        systemCheckForExercise()
        
        logger.info("Running the auction checker...")
        systemCheckForAuctionClosings()
        
        logger.info("Updating the rankings...")
        systemRecalculateRankings()
        
        logger.info("Checking for dividends")
        systemCheckForDividends()
    }
}

