
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import scala.xml.{NodeSeq}

import model._
import model.schema._

object portfolio {
//

def apply(port: Portfolio, currentUser: Option[User], modifiable: Boolean) =
    <div>
        {tChart(port, modifiable=modifiable, currentUser=currentUser)}
        {stockPlot(port)}
        {stockChart(port, modifiable)}
        {dividendChart(port, modifiable)}
    </div>

//
}

object stockPlot {

def apply(port:Portfolio): NodeSeq = {
    import scala.xml.NodeSeq
    import net.liftweb.util.Helpers._
    import net.liftweb.http.js.JsCmds._
    import net.liftweb.widgets.flot._

    val data_values: List[(Double,Double)] = for (i <- List.range (0, 140, 5))
    yield (i / 10.0, Math.sin(i / 10.0) ) 

    val data_to_plot = new FlotSerie() {
        override val data = data_values
    }

    val flot1 = <div/>
    val flot2 = Flot.render("portfolio_plot", List(data_to_plot), new FlotOptions {}, Flot.script(flot1))

    (flot1 ++
     flot2 ++
     <div class="block">
        <h2>Portfolio Net Worth</h2>
        <div id="portfolio_plot" style="width:100%;height:200px;"></div>
     </div>)
}

}
