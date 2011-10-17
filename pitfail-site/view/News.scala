
package code
package comet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import control.LoginManager
import lib.formats._

import net.liftweb.actor
import actor._

class News extends Refreshable
    with Loggable
{
    import model.Schema._
    
    def registerWith = News
    
    override def render = doRender _
    
    def doRender(in: NodeSeq) = trans {
        val events: Seq[NewsEvent] = recentEvents(10)
        
        val headlines = events map { ev =>
            ev.action match {
                case "buy" => (
                      "choice=blank [choice]" #> "buy"
                    & "user=blank [user]" #> ev.subject.username
                    & "#ticker *" #> ev.ticker
                    & "#volume *" #> (ev.price toDollarString)
                )
                case "sell" => (
                      "choice=blank [choice]" #> "sell"
                    & "user=blank [user]" #> ev.subject.username
                    & "#ticker *" #> ev.ticker
                    & "#volume *" #> (ev.price toDollarString)
                )
            }
        }
        
        ("#newsItem *" #> headlines)(in)
    }
}

object News extends RefreshHub

