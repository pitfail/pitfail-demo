
// Written by: Michael Koval
// Written by: Owen Healy

package code
package snippet

import net.liftweb.{common, http, util}
import common._
import scala.xml._

import net.liftweb.widgets.flot._
import http.js.JsCmds._
import http.js._
import http.js.JE._

import model._
import model.schema._

object holdingsPlot extends Loggable {

def apply() : NodeSeq = apply(League.default)

def apply(league: League): NodeSeq = readDB {
//
    
val maxShow = 8

lazy val main = {
    val holdings = leagueSH(league) sortBy (- _.dollars) take maxShow
    if (holdings.isEmpty) Nil
    else rest(holdings)
}

def rest(holdings: List[StockHolding]) = {
    val items = (1 to holdings.length) zip holdings

    val serie = new FlotSerie() {
        override val data: List[(Double, Double)] =
            items map { case (n, holding) =>
                (n.doubleValue(), holding.dollars.double)
            } toList
    }

    val dollars = holdings map (_.dollars.double)
    val range  = (dollars.min, dollars.max)

    val options:FlotOptions = new FlotOptions () {
        override def buildOptions = List(
            Full("series" -> JsObj(
                "points" -> JsObj("show" -> JsFalse),
                "lines"  -> JsObj("show" -> JsFalse),
                "bars"   -> JsObj("show" -> JsTrue, "align" -> Str("center"))
            )),
            Full("xaxis" -> JsObj(
                "min"   -> Num(0.5),
                "max"   -> Num(maxShow),
                "ticks" -> JsArray(
                    items map { case (n,  holding) =>
                        JsArray(Num(n), Str(holding.ticker))
                    } toList
                )
            )),
            Full("yaxis" -> JsObj(
                "min" -> Num(0.0),
                "max" -> Num(range._2)
            ))
        )
    }

    val flot1 = <div/>
    val flot2 = Flot.render("holdings_plot", List(serie), options, Flot.script(flot1))

    val html =
        flot1 ++
        flot2 ++
        <div class="block">
           <h2>Stock Holdings in PitFail</h2>
           <div id="holdings_plot" style="width:100%;height:200px;"></div>
        </div>
            
    logger.info(html)

    html
}

main
//
}

}

