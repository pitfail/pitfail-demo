
package code
package snippet

import net.liftweb.common.Loggable
import scala.xml._

import intform._

import net.liftweb.http._
import js._
import JsCmds._
import JE._

import model._
import model.schema._

class HighestVotedTrades extends Page with Loggable {
    def render = {
        lazy val main =
            <div class="block">
                <table>
                    <tr><td>Score</td><td>Description</td></tr>
                    {rows}
                </table>
            </div>
        
        lazy val rows = highestVotedEvents(10) map { event =>
            <tr>
                <td>{event.score}</td>
                <td>{comet.News.eventDescription(event, link=true)}</td>
            </tr>
        }
        
        main
    }
}

