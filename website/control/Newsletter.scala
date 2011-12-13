
// Written by: Owen Healy

package code
package control

import net.liftweb.{util, common}
import util._
import util.Helpers._
import common.Loggable

import model._ 
import model.schema._

object Newsletter extends Loggable {
    
    def run(): Unit = periodically(1 weeks) {
        runNewsletter()
    }
    
    def runNewsletter() {
        logger.info("Sending the newsletter...")
        
        subscriptions.toList map { subscr =>
            val user  = subscr.user: User
            val email = subscr.email
            logger.info("Sending to %s at %s" format (user.username, email))
            
            newsletter.send_newsletter.send_newsletter(user.username, email);
        }
        
        logger.info("Sent!")
    }
}


