package code
package snippet

import net.liftweb.{common, http}
import common._
import http._

import scala.xml._

import intform._
import errors._
import Box._

import formats._
import model.schema._

import net.liftweb.util.Helpers.tryo

/*
class LeaderSnippet extends Snippet with Loggable {
    def render = {
        <p>hi</p>
    }
}
*/

class LeaderPage extends Page with Loggable {

    def paramAsBigInt(param_name: String) : Box[BigInt] = {
        S param param_name flatMap { x:String => tryo(BigInt(x))}
    }

    val startParam  = paramAsBigInt("start").openOr(BigInt(0))
    val countParam  = paramAsBigInt("count").openOr(BigInt(50))
    val leagueParam = S param "league" openOr "default"

    def render = try readDB {
        val start = startParam intValue
        val count = countParam intValue
        val league_n = leagueParam

        val last = start + count - 1

        val ml  = League byName league_n
        val p1 = for {
            l <- ml.toList
            p <- Portfolio.byLeague(l).sortBy(_.rank).drop(start).take(count)
        } yield p

        val n : NodeSeq = for {
            p <- p1
        } yield {
            <tr>
                <td>{p.rank}</td>
                <td>{p.name}</td>
                <td>{p.cash.$}</td>
            </tr>
        }

        val stuff = ml match {
            case Some(l) =>
            if (l.name == League.defaultName)
            <h2>Ranked Portfolios in League "{l.name}"
                {if (start != 0)
                    <span> ({start} to {last})</span>
                }
            </h2>
            <table>
                <tr><th>Rank</th><th>Portfolio</th><th>Cash</th></tr>
                {n}
            </table>
            case None =>
            <p>No such league {league_n}</p>
        }

        <div id="leader-page" class="block">
            {stuff}
        </div>

    } catch {
        case e => <p>Sorry, {standardMessage(e)}</p>
    }
}

