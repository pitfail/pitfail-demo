
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
        {ordersChart(port, modifiable)}
    </div>

//
}

