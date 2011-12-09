
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import scala.xml.{NodeSeq}

import model._
import model.schema._

object leaderboardPlot {


def apply(portfolios: List[Portfolio]): NodeSeq = readDB {
    import scala.math.{ceil,floor}
    import scala.xml.NodeSeq
    import net.liftweb.http.js.JsCmds._
    import net.liftweb.widgets.flot._
    import net.liftweb.common.Full
    import org.joda.time.DateTime
    import org.scala_tools.time.Imports._
    import net.liftweb.http.js._
    import net.liftweb.http.js.JE._

    val TIME_WINDOW = (7 days)
    val MIN_SCALE = Scale(0.8)
    val MAX_SCALE = Scale(1.2)

    val begin   = DateTime.now - TIME_WINDOW
    val end     = DateTime.now

    val histories = portfolios map { portfolio =>
        (schema.PortfolioValues.history(portfolio, begin, end).toList
            sortBy (_._1.getMillis))
    }

    val data = (portfolios zip histories) map {
        case (portfolio, history) => new FlotSerie() {
            override val data: List[(Double, Double)] =
                history map {
                    // TODO: Convert this to the user's timezone.
                    case (updateTime: DateTime, dollars: Dollars) =>
                        (updateTime.getMillis().toDouble, dollars.dollars.toDouble)
                }
        }
    }

    // List[List[(DateTime, Dollars)]]
    val getDollars = (x: List[(Any, Dollars)]) => x map (_._2)
    val dollarsMin = MIN_SCALE * {try { (histories map { getDollars(_).min }).min } catch { case _ => Dollars(0) }}
    val dollarsMax = MAX_SCALE * {try { (histories map { getDollars(_).max }).max } catch { case _ => Dollars(10) }}

    val options:FlotOptions = new FlotOptions () {
        override val series = Full(Map( 
            "points" -> JsObj(
                "show"      -> JsTrue,
                "radius"    -> Num(3),
                "symbol"    -> "circle",
                "fill"      -> JsFalse
            ),
            "lines" -> JsObj(
                "show"      -> JsTrue,
                "steps"     -> Num(1),
                "fill"      -> JsFalse
            )
        ))

        override val xaxis = Full(new FlotAxisOptions() {
            override val min   = Full(begin.getMillis().toDouble)
            override val max   = Full(end.getMillis().toDouble)
            override val mode  = Full("time")
        })

        override val yaxis = Full(new FlotAxisOptions() {
            override val min = Full(dollarsMin.dollars.toDouble)
            override val max = Full(dollarsMax.dollars.toDouble)
        })

        override val legend = Full( new FlotLegendOptions() {
            override val container = Full("legend_area")                            
        })
    }


    val flot1 = <div/>
    val flot2 = Flot.render("portfolio_plot", data, options, Flot.script(flot1))

    (flot1 ++
     flot2 ++
     <div class="block">
        <h2>Portfolio Net Worth</h2>
        <div id="portfolio_plot" style="width:100%;height:200px;"></div>
     </div>)
}

def getRange(prices: Seq[Dollars]): (Double, Double) = {
    import scala.math.{ceil,floor}

    try {
        if (prices.isEmpty) {
            (0.0, 10.0)
        } else {
            val min = prices.min.dollars.toDouble
            val max = prices.max.dollars.toDouble
            val range = max - min
            val extra = (10.0 - range) / 2

            if (max - min < 10.0)
                (floor(min - extra), ceil(max + extra))
            else
                (floor(min), ceil(max))
        }
    } catch {
        case e : Exception =>
            (0, 10)
    }
}

}
