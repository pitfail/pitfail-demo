
package texttrading

import dispatch._
import dispatch.Request._
import dispatch.oauth._
import dispatch.twitter._
import dispatch.json.JsHttp._
import net.liftweb.json.JsonAST._

import net.liftweb.common.Loggable

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
    logger.info("Sending the following to the user %S: %S" format(user, message))
    h(twitter.Status.update("@" + user + " " + message, consumer, token) >~ { src =>
      logger.info("STU: " + src.getLines.mkString)
    })
  }

  def follow_user(user:String) = {
    /* TODO: follow the specfied user */
  }

  val user_stream = UserStream.open(consumer, token, None, FollowMutual, Seq("@" + app.accessUser)) {
      message : JValue =>

    for {
      JString(line) <- message \ "text"
      JString(user) <- message \ "user" \ "screen_name"
    } yield {

      logger.info("... %s: %s" format(user, line))
      if (user != app.accessUser) {
        follow_user(user)
        send_to_user(user, TextTrader.runCommand(user, line, backend) mkString (" "))
      }
    }
  }

  def run() = {
    logger.info("started the user stream")
    h(user_stream)
  }
}

