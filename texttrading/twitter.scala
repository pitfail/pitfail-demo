
package texttrading

import dispatch._
import dispatch.Request._
import dispatch.oauth._
import dispatch.twitter._
import dispatch.json.JsHttp._
import net.liftweb.json.JsonAST._

import net.liftweb.common.Loggable
import net.liftweb.util.Schedule
import net.liftweb.actor.LiftActor
import net.liftweb.util.Helpers._

import code.keys.RWTwitter

object twit {
  val twit = new TwitterFrontend(RWTwitter, new PitFailBackend)
  def run() = twit.run()
}

class TwitterFrontend(
    app : {
        val consumerKey: String
        val consumerSecret: String
        val accessToken: String
        val accessTokenSecret: String
        val accessUser: String
    },
    backend: Backend
) extends Loggable
{
    val h  = new nio.Http
    val consumer = Consumer(app.consumerKey, app.consumerSecret)
    val token = Token(app.accessToken, app.accessTokenSecret)

    def send_to_user(user: String, message: String) = {
        logger debug("Sending the following to the user %s: %s" format(user, message))
        h(twitter.Status.update("@" + user + " " + message, consumer, token) >~ { src =>
            logger debug("Twitter status update returned: " + src.getLines.mkString)
        })
    }

    def follow_user(user:String) = {
        /* TODO: follow the specfied user */
    }

    var last_id = 0

    val run_again = new LiftActor {
        def messageHandler = {
            case _ => run()
        }
    }

    val user_stream = UserStream.open(consumer, token, None, RepliesMutual, Seq("@" + app.accessUser)) {
        message : JValue =>

        for {
            JString(line) <- message \ "text"
            JString(user) <- message \ "user" \ "screen_name"
            /* TODO: match the id and update last id */
        } yield {
            logger info("Twitter user stream recived %s: %s" format(user, line))
            /* TODO: verify the id advanced by 1, backfill otherwise. At the least, don't
             * process tweets more than once */
            if (user != app.accessUser) {
                follow_user(user)
                TextTrader.runCommand(user, line, backend) foreach { send_to_user(user, _) }
            }
        }
    } ^! {
        case e =>
            /* assume something bad happend */
            logger error("user stream error " + e)
            Schedule.schedule(run_again, 'yo, 5 seconds)
    }

    def run() {
        h(user_stream)
        logger.info("started the user stream")
    }
}

