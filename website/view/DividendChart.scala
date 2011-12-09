
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

object dividendChart extends Loggable {
//

def apply(port: Portfolio, modifiable: Boolean): NodeSeq = {
//
    
val payments = port.myDividendPayments

lazy val main =
    <div class="block">
        <h2>Dividend payments</h2>
    </div>
    
lazy val content =
    if (payments.isEmpty) <p>None</p>
    else
        <table>
            <tr><td>Date</td><td>Stock</td><td>Amount</td></tr>
            {rows}
        </table>

lazy val rows = payments map { pay =>
    <tr>
        <td>{pay.date.toNearbyString}</td>
        <td>{pay.ticker}</td>
        <td>{pay.dollars}</td>
    </tr>
}

main
}

//
}


