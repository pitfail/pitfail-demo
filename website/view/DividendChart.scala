
// Written by: Owen Healy
// Written by: Michael Koval

package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import formats._
import intform._

import control.LoginManager
import LoginManager.{currentLogin}
import stockdata._
import org.joda.time.Duration

import model._
import model.schema._

// ref_44
object dividendChart extends Loggable {
//

def apply(port: Portfolio, modifiable: Boolean): NodeSeq = {
//
    
val payments = port.myDividendPayments

lazy val main =
    <div class="block">
        <h2>Dividend payments</h2>
        {content}
    </div>
    
lazy val content =
    if (payments.isEmpty) <p>None</p>
    else
        <table class="boxy">
            <thead>
                <tr><th>Date</th><th>Stock</th><th>Amount</th></tr>
            </thead>
            <tbody>
                {rows}
            </tbody>
        </table>

lazy val rows = payments filter (_.dollars > Dollars(0)) map { pay =>
    <tr>
        <td>{pay.date.toNearbyString}</td>
        <td>{pay.ticker}</td>
        <td>{pay.dollars.$}</td>
    </tr>
}

main
}

//
}


