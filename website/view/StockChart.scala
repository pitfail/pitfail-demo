
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

object stockChart extends Loggable {
//

def apply(port: Portfolio, modifiable: Boolean): NodeSeq = {
//
    
lazy val main =
    <div class="block">
        <h2>Stocks</h2>
        {stockTable}
    </div>

lazy val stockTable =
    <table>
        <tr>
            <td colspan="2"/>
            <td colspan="2">Purchased</td>
            <td colspan="2"/>
        </tr>
        <tr>
            <td>Ticker</td>
            <td>Shares</td>
            <td>on</td>
            <td>at</td>
            <td>Price</td>
            <td>Dividends</td>
        </tr>
        {tableRows}
    </table>

lazy val tableRows = port.myStockAssets map { asset =>
    <tr>
        <td>{asset.ticker}</td>
        <td>{asset.shares.###()}</td>
        <td>{asset.purchaseDate.toNearbyString}</td>
        <td>{asset.purchasePrice.$}</td>
        <td>{Stocks.stockPrice(asset.ticker).$}</td>
        <td>{asset.totalDividends.$}</td>
    </tr>
}

main
}

//
}

