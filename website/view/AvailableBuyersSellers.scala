
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

import control.LoginManager
import formats._
import intform._

import LoginManager.{currentLogin}
import stockdata._
import org.joda.time.Duration

import model._
import model.schema._

object availableSellers extends Loggable {
//

def apply(ticker: String) = {
    val sellers = schema.sellersFor(ticker)
    val rows = sellers map { seller =>
        <tr>
            <td>{seller.price.$}</td>
            <td>{(seller.available*seller.price).$}</td>
            <td>{seller.available.###()}</td>
            <td>{seller.name}</td>
        </tr>
    }
    
    <div class="available-sellers">
        <table class="boxy">
            <thead>
                <tr>
                    <th>Asking Price</th>
                    <th>Dollars</th>
                    <th>Shares</th>
                    <th>Name</th>
                </tr>
            </thead>
            <tbody>{rows}</tbody>
        </table>
    </div>
}

//
}

object availableBuyers extends Loggable {
//

def apply(ticker: String) = {
    val buyers = schema.buyersFor(ticker)
    val rows = buyers map { buyer =>
        <tr>
            <td>{buyer.price.$}</td>
            <td>{(buyer.available*buyer.price).$}</td>
            <td>{buyer.available.###()}</td>
            <td>{buyer.name}</td>
        </tr>
    }
    
    <div class="available-buyers">
        <table class="boxy">
            <thead>
                <tr>
                    <th>Bid Price</th>
                    <th>Dollars</th>
                    <th>Shares</th>
                    <th>Name</th>
                </tr>
            </thead>
            <tbody>{rows}</tbody>
        </table>
    </div>
}

//
}

