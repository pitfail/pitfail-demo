
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

object ordersChart extends Loggable {
//

def apply(port: Portfolio, modifiable: Boolean): NodeSeq = readDB {
//

lazy val refreshable = Refreshable(doRender)

def doRender: NodeSeq = {
    val myBuyLimitOrders = port.myBuyLimitOrders
    val mySellLimitOrders = port.mySellLimitOrders

    lazy val main =
        <div class="block">
            <h2>Pending Buy Orders</h2>
            {ordersTable(myBuyLimitOrders)}
            <h2>Pending Sell Orders</h2>
            {ordersTable(mySellLimitOrders)}
        </div>

    def ordersTable(orders: List[OrderAttrs]) = {
        lazy val table =
            <table>
                <tr>
                    <td>Ticker</td>
                    <td>Shares Outstanding</td>
                    <td>Limit Price</td>
                    <td>Last Trade Price</td>
                    <td/>
                </tr>
                {tableRows(orders)}
            </table>
        
        if (!orders.isEmpty) table
        else <p class="none">You have none.</p>
    }

    def tableRows(orders: List[OrderAttrs]) = orders map { order =>
        val lastTradePrice = Stocks.lastTradePrice(order.ticker)
        val limitPrice = order.limit
        val sharesOutstanding = order.shares
        val ticker = order.ticker
        
        val cancelButton = FormSubmit.rendered("Cancel") {
            try {
                order.userCancel()
                refreshable.refresh
            }
            catch {
                case _: errors.BadUser => throw BadInput("Dunno...")
            }
        }
        
        <tr>
            <td>{ticker}</td>
            <td>{sharesOutstanding.###()}</td>
            <td>{limitPrice.$}/sh</td>
            <td>{lastTradePrice.$}/sh</td>
            <td>{if (modifiable) cancelButton else Nil}</td>
        </tr>
    }

    main
}

refreshable.render
}

//
}

