
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

def apply(port: Portfolio, modifiable: Boolean): NodeSeq = readDB {
//
    
val myStockAssets = port.myStockAssets
val haveAny = !myStockAssets.isEmpty

lazy val main =
    <div class="block">
        <h2>Stocks</h2>
        {if (haveAny) stockTable else <p class="none">You have no stocks</p>}
        {seller.render}
    </div>

lazy val stockTable =
    <table class="boxy">
        <thead>
            <tr>
                <th>Ticker</th>
                <th/>
                <th>Shares</th>
                <th>Last Price</th>
                <th>Dividends</th>
                <th/>
            </tr>
        </thead>
        <tbody>
            {tableRows}
        </tbody>
    </table>

lazy val tableRows = port.myStockAssets map { asset =>
    val price = Stocks.lastTradePrice(asset.ticker)
    
    val sellButton = FormSubmit.rendered("Sell...") {
        seller.activate(asset.ticker, asset.shares * price)
    }
    
    val top = (
        <tr class="stock-chart-stock">
            <td>{asset.ticker}</td>
            <td/>
            <td>{asset.shares.###()}</td>
            <td>{price.$}</td>
            <td>{asset.totalDividends.$}</td>
            <td>{if (modifiable) sellButton else Nil}</td>
        </tr>
    )
    
    val bought = asset.buyHistories map { hist =>
        <tr class="stock-history">
            <td/>
            <td class="stock-history-action">bought</td>
            <td>{hist.shares.###()}</td>
            <td>{hist.buyPrice.$}</td>
            <td>{hist.buyDate.toNearbyString}</td>
            <td/>
        </tr>
    }
    
    val sold = asset.sellHistories map { hist =>
        <tr class="stock-history">
            <td/>
            <td class="stock-history-action">sold</td>
            <td>{hist.shares.###()}</td>
            <td>{hist.sellPrice.$}</td>
            <td>{hist.sellDate.toNearbyString}</td>
            <td/>
        </tr>
    }
    
    top ++ bought ++ sold ++ <tr class="gap"><td/><td/><td/><td/><td/><td/></tr>
}

lazy val seller = StockSeller()

main
}

//
}

