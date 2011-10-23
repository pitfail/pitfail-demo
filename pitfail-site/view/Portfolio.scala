
package code
package comet

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
import lib.formats._
import intform._

import model.Schema._
import LoginManager.{currentLogin}

class Portfolio extends Refreshable with Loggable
{
    def registerWith = Portfolio
    
    def render = (in: NodeSeq) => trans {
        for {
            name <- currentLogin
            user <- byUsername(name)
            
            port = user.mainPortfolio
            
            myStockAssets           = port.myStockAssets
            myCashAmount            = port.cash
            myDerivativeAssets      = port.myDerivativeAssets
            myDerivativeLiabilities = port.myDerivativeLiabilities
        } yield
        {
            def result =
                <table class="tChart">
                    <col class="left"/>
                    <col class="right"/>
                    <tr class="header">
                        <th>Assets</th>
                        <th>Liabilities</th>
                    </tr>
                    <tr>
                        <td class="assets">
                            {cash}<br/>
                            {stocks}<br/>
                            {derivativeAssets}
                        </td>
                        <td class="liabilities">
                            {derivativeLiabilities}
                        </td>
                    </tr>
                </table>
            
            def cash = <p>{myCashAmount.$}</p>
            
            def stocks = (
                <h4>Stocks</h4> ++ {
                    if (!myStockAssets.isEmpty) stockTable
                    else <p>You have no stocks</p>
                }
            )
            
            def stockTable = (
                <table class="stocksTable">
                    <tr>
                        <th>Ticker</th>
                        <th>Volume</th>
                    </tr>
                    { myStockAssets map stock _ }
                </table>
            )
            
            def stock(sa: StockAsset) =
                <tr>
                    <td>{sa.ticker}</td>
                    <td>{stockVolume(sa).$}</td>
                    <td>{snippet.SellThisStock(sa.ticker)}</td>
                </tr>
            
            def derivativeAssets = (
                <h4>Derivatives</h4> ++ {
                    if (!myDerivativeAssets.isEmpty) {
                        <table class="derivativesTable">
                            <tr>
                                <th>Securities</th>
                                <th>Exec Date</th>
                                <th>Condition</th>
                                <th>Peer</th>
                            </tr>
                            { myDerivativeAssets map derivativeAsset _ }
                        </table>
                    }
                    else {
                        <p>You have no derivatives</p>
                    }
                }
            )
            
            def derivativeAsset(da: DerivativeAsset) = {
                val deriv = da.derivative
                val peer  = da.peer.owner.owner
                
                <tr>
                    <td>{deriv.securities toHumanString}</td>
                    <td>{deriv.exec toNearbyString}</td>
                    <td>{deriv.condition toHumanString}</td>
                    <td>{snippet.UserLink(peer.username)}</td>
                    <td> {
                        if (deriv.early) execDerivative(da)
                        else Nil
                    } </td>
                </tr>
            }
            
            def derivativeLiabilities =
                <h4>Derivatives</h4> ++ {
                    if (!myDerivativeLiabilities.isEmpty)
                        derivativeLiabilityTable
                    else
                        <p>You have no derivative liabilities</p>
            }
        
            def derivativeLiabilityTable =
                <table>
                    <tr>
                        <th>Securities</th>
                        <th>Exec Date</th>
                        <th>Condition</th>
                    </tr>
                    { myDerivativeLiabilities map derivativeLiability _ }
                </table>
            
            def derivativeLiability(dl: DerivativeLiability) = {
                val deriv = dl.derivative
                
                <tr>
                    <td>{deriv.securities toHumanString}</td>
                    <td>{deriv.exec toNearbyString}</td>
                    <td>{deriv.condition toHumanString}</td>
                    <td> {
                        if (dl.remaining < 1) dl.remaining.%()
                        else Nil
                    } </td>
                </tr>
            }
            
            result
        }
    } getOrElse <p>Login to view your portfolio</p>
    
    def execDerivative(da: DerivativeAsset) =
        Submit("Exec") {
            da.refetch() map {execute _} match {
                case Some(_) =>
                    Portfolio ! Refresh
                    Noop
                case None => throw BadInput("No longer exists")
            }
        }
    
    def execute(da: DerivativeAsset) {
        try {
            da.execute()
        }
        catch {
            case NotExecutable => throw BadInput("Not executable")
        }
    }
    
    // TODO: This is not right
    def stockVolume(stock: StockAsset): BigDecimal =
        BigDecimal("3.14")
}

object Portfolio extends RefreshHub

