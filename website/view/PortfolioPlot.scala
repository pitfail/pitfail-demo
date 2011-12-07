
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import scala.xml.{NodeSeq}

import model._
import model.schema._

object stockPlot {

def apply(portfolio: Portfolio): NodeSeq = {
    import scala.math.{ceil,floor}
    import scala.xml.NodeSeq
    import net.liftweb.http.js.JsCmds._
    import net.liftweb.widgets.flot._
    import net.liftweb.common.Full
    import org.joda.time.DateTime
    import org.scala_tools.time.Imports._
    import net.liftweb.http.js._
    import net.liftweb.http.js.JE._

    val begin   = DateTime.now - (7 days)
    val end     = DateTime.now
    val history = schema.PortfolioValues.history(portfolio, begin, end).toList

    val data_to_plot = new FlotSerie() {
        override val data: List[(Double, Double)] =
            history map {
                // TODO: Convert this to the user's timezone.
                case (updateTime: DateTime, dollars: Dollars) =>
                    (updateTime.getMillis().toDouble, dollars.dollars.toDouble)
            }
    }

    val prices = history map { case (_, dollars: Dollars) => dollars }
    val range  = getRange(prices)
    println(range)

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
                "fill"      -> JsTrue
            )
        ))

        override val xaxis = Full(new FlotAxisOptions() {
            override val min   = Full(begin.getMillis().toDouble)
            override val max   = Full(end.getMillis().toDouble)
            override val mode  = Full("time")
        })

        override val yaxis = Full(new FlotAxisOptions() {
            override val min = Full(range match { case (min, _) => min })
            override val max = Full(range match { case (_, max) => max })
        })

        override val legend = Full( new FlotLegendOptions() {
            override val container = Full("legend_area")                            
        })
    }


    val flot1 = <div/>
    val flot2 = Flot.render("portfolio_plot", List(data_to_plot), options, Flot.script(flot1))

    (flot1 ++
     flot2 ++
     <div class="block">
        <h2>Portfolio Net Worth</h2>
        <div id="portfolio_plot" style="width:100%;height:200px;"></div>
     </div>)
}

def getRange(prices: Seq[Dollars]): (Double, Double) = {
    import scala.math.{ceil,floor}

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
}

}
