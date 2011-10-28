
package code
package control

import net.liftweb.{util, common}
import util._
import util.Helpers._
import common.Loggable

object Checker extends Loggable {
    
    def run(): Unit = periodically(30 minutes) {
        runChecks()
    }
    
    def runChecks() {
        import comet._
        
        logger.info("Running the derivative checker...")
        model.Schema.checkForExercise()
        
        logger.info("Running the auction checker...")
        model.Schema.checkForAuctionClosings()
        
        Portfolio        ! Refresh
        News             ! Refresh
        AuctionThumbnail ! Refresh
        Offers           ! Refresh
        OutgoingOffers   ! Refresh
    }
}

