
package email

import scala.actors.Actor
import Actor._

object EmailActor extends Actor {
    
    def act() {
        while (true) receive {
            case (to: String, subject: String, body: String) =>
                Email_bg send_email (to, subject, body)
        }
    }
}

