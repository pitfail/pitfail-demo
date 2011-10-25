
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
import formats._
import intform._

import model._
import model.Schema._
import LoginManager.{currentLogin}
import stockdata._
import org.joda.time.Duration

class Portfolio extends Refreshable with Loggable
{
    // TODO: This should be a singleton object to take full advantage of
    //       caching.
    private val stockDatabase: StockDatabase = new CachedStockDatabase(
        new YahooStockDatabase(new HttpQueryService("GET")),
        // TODO: This timeout should be moved to a configuration file.
        new Duration(1000 * 60 * 5)
    )

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
        } yield {
            def result =
                <div id="portfolio" class="block container portfolio">
                    <h2>Portfolio</h2>
                    <table class="tchart assets">
                        <thead>
                            <tr><th colspan="3"><h3>Assets</h3></th></tr>
                        </thead>
                        <tbody>
                            <tr>
                                <th>Cash</th>
                                <td>{myCashAmount.$}</td>
                                <td/>
                            </tr>

                            <tr><th colspan="3">Stocks</th></tr>
                            {stocks}

                            <tr><th colspan="3">Derivatives</th></tr>
                            {derivativeAssets}
                        </tbody>
                    </table>
                    
                    <table class="tchart liabilities">
                        <thead>
                            <tr><th colspan="3"><h3>Liabilities</h3></th></tr>
                        </thead>
                        <tbody>
                            <tr><th colspan="3">Derivatives</th></tr>
                            {derivativeLiabilities}
                        </tbody>
                    </table>
                    <div style="clear:both;"/>
                </div>

            def stocks =
                if (myStockAssets isEmpty)
                    <tr><td colspan="3">none</td></tr>
                else
                    {myStockAssets map { (asset) => {
                        <tr>
                            <td>{asset.ticker}</td>
                            <td>{stockVolume(asset).$}</td>
                            <td class="buttons">{snippet.SellThisStock(asset.ticker)}</td>
                        </tr>
                    }}}
            
            def derivativeAssets =
                if (myDerivativeAssets isEmpty)
                    <tr><td colspan="3">none</td></tr>
                else
                    {myDerivativeAssets map { (asset) => {
                        <tr>
                            <td colspan="2">{asset.derivative toHumanString}</td>
                            <td class="buttons">{
                                if (asset.derivative.early)
                                    execDerivative(asset)
                                else
                                    Nil
                            }</td>
                        </tr>
                    }}}
            
            def derivativeLiabilities =
                if (myDerivativeLiabilities isEmpty)
                    <tr><td colspan="3">none</td></tr>
                else
                    {myDerivativeLiabilities map { (liability) => {
                        <tr>
                            <td colspan="2">{liability.derivative toHumanString}</td>
                            <td>{
                                if (liability.remaining < Scale("1"))
                                    liability.remaining.%()
                                else
                                    Nil
                            }</td>
                        </tr>
                    }}}
            
            result
        }
    } getOrElse <p>Login to view your portfolio</p>
    
    def execDerivative(da: DerivativeAsset) = FormSubmit.rendered("Execute") {
        da.refetch() map {execute _} match {
            case Some(_) =>
                Portfolio ! Refresh
                Noop
            case None => throw BadInput("No longer exists")
        }
    }
    
    def execute(da: DerivativeAsset) {
        try {
            da.executeManually()
        }
        catch {
            case NotExecutable => throw BadInput("Not executable")
        }
    }
    
    // TODO: This is not right
    def stockVolume(asset: StockAsset): Dollars = {
        val shares = asset.shares
        val stock  = Stock(asset.ticker)
        val quote  = stockDatabase.getQuotes(Seq(stock)).head
        shares * quote.price
    }
}

object Portfolio extends RefreshHub

