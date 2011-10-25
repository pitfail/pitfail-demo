
package texttrading

import dispatch._
import dispatch.Request._
import dispatch.oauth._
import dispatch.twitter._
import dispatch.json.JsHttp._
import net.liftweb.json.JsonAST._

import net.liftweb.common.Loggable

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
  val h = new nio.Http with NoLogging
  val consumer = Consumer(app.consumerKey, app.consumerSecret)
  val token = Token(app.accessToken, app.accessTokenSecret)

  def send_to_user(user: String, message: String) = {
    h(twitter.Status.update("@%s " format user + message, consumer, token) >>> System.out)
  }

  def follow_user(user:String) = {
    /* TODO: follow the specfied user */
  }

  val user_stream = UserStream.open(consumer, token, None, FollowAll) { message : JValue =>
    logger.info("[US RECV] " + message)

    for {
      JString(line) <- message \ "text"
      JString(user) <- message \ "user" \ "screen_name"
    } yield {
      send_to_user(user, TextTrader.runCommand(user, line, backend) mkString (" "))
    }
  }

  def run() = {
    h(user_stream)
  }
}

