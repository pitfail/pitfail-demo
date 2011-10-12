
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
import org.squeryl.PrimitiveTypeMode.inTransaction

import control.LoginManager
import lib.formats._

import model.{Schema}
import Schema._

import net.liftweb.actor
import actor._

class News extends CometActor
    with CometListener
    with Loggable
{
    def registerWith = NewsHub
    
    override def lowPriority: PartialFunction[Any,Unit] = {
        case Refresh => reRender()
    }
    
    override def render = doRender _
    
    def doRender(in: NodeSeq) = inTransaction {
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

object NewsHub extends LiftActor
    with ListenerManager
{
    def createUpdate = Refresh
    
    override def lowPriority = {
        case Refresh => updateListeners()
    }
    
    def apply() = this ! Refresh
}

