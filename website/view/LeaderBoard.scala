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

        val topPlayers = for {
            l <- ml.toList
            p <- Portfolio.byLeague(l).sortBy(_.rank).take(5)
        } yield p

        val n : NodeSeq = for {
            p <- p1
        } yield {
            <tr>
                <td>{p.rank}</td>
                <td>{PortfolioLink(p)}</td>
                <td>{p.spotValue.$}</td>
            </tr>
        }

        val stuff = ml match {
            case Some(l) =>
            val league_ref = if (l ~~ League.default) {
                "the Default League"
            } else {
                "League " + l.name
            }

            import stockdata.{HttpQueryService => HQS}
            val base = Map("count" -> count.toString, "league" -> league_n.toString)
            val next_link = "?" + HQS.buildQuery(Map("start" -> (last + 1).toString) ++ base, "UTF-8")
            val prev_link = "?" + HQS.buildQuery(Map("start" -> (start - count).toString) ++ base, "UTF-8")

            <h2>Ranked Portfolios in {league_ref}
                {if (start != 0)
                    Text("(" + start + " to " + last + ")")
                }
            </h2>
            <table class="boxy">
                <tr><th>Rank</th><th>Portfolio</th><th>Cash</th></tr>
                {n}
            </table>
            <p>
                <a href={prev_link} rel="prev">prev</a>
                <a href={next_link} rel="next">next</a>
            </p>
            case None =>
            <p>No such league {league_n}</p>
        }

        <lift:children>
            <div id="leader-page" class="block">
                {stuff}
            </div>
            {leaderboardPlot(topPlayers)}
            {holdingsPlot()}
        </lift:children>

    } catch {
        case e => <p>Sorry, {standardMessage(e)}</p>
    }
}

