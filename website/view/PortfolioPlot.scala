
package code
package snippet

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds._
import net.liftweb.widgets.flot._

class PortfolioPlot {
    def plot(): NodeSeq = {
        val data_values: List[(Double,Double)] = for (i <- List.range (0, 140, 5))
        yield (i / 10.0, Math.sin(i / 10.0) ) 

        val data_to_plot = new FlotSerie() {
            override val data = data_values
        }

        val node = <div/>
        node ++ Flot.render ( "graph_area", List(data_to_plot), new FlotOptions {}, Flot.script(node))
    }
}

