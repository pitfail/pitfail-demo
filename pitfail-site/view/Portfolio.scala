
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

import model.{Schema}
import Schema.{User, StockAsset, DerivativeAsset, byUsername}
import LoginManager.{currentLogin}
import org.squeryl.PrimitiveTypeMode.inTransaction

class Portfolio extends Refreshable with Loggable
{
    def registerWith = Portfolio
    
    // Things to show
    var myStockAssets: Seq[StockAsset] = Nil
    var myCashAmount: BigDecimal = BigDecimal("0.00")
    var myDerivativeAssets: Seq[DerivativeAsset] = Nil
    
    override def refresh(): Unit = inTransaction {
        for {
            name <- currentLogin
            user <- byUsername(name)
        } {
            val port = user.mainPortfolio
            
            myStockAssets      = port.myStockAssets
            myCashAmount       = port.cash
            myDerivativeAssets = port.myDerivativeAssets
        }
    }
    refresh()
    
    def render = (
          cash
        & ifHaveStocks(stocks)
        & ifNoStocks
        & ifHaveDerivativeAssets(derivativeAssets)
        & ifNoDerivativeAssets
    )
    
    private def cash = "#cash *" #> (myCashAmount toDollarString)
    
    private def stocks =
        "#stock *" #> (myStockAssets map ( a =>
              "#ticker *" #> a.ticker
            & "#volume *" #> (stockVolume(a) toDollarString)
            & "name=ticker [value]" #> a.ticker
        ))
    
    private def derivativeAssets =
        "#derivativeAsset" #> (myDerivativeAssets map {a =>
            val deriv = a.derivative
            (
                  "#securities *" #> (deriv.security toHumanString)
                & "#exec *"       #> (deriv.exec toString)
                & "#condition *"  #> (deriv.condition toHumanString)
                & "#peer *"       #> ("user= [user]" #> a.peer.owner.username)
            )
        })
    
    private def ifHaveStocks(next: CssBindFunc) =
        "#ifHaveStocks" #> { n =>
            if (!myStockAssets.isEmpty) next(n)
            else Nil
        }
    
    private def ifNoStocks =
        "#ifNoStocks" #> { n =>
            if (myStockAssets.isEmpty) n
            else Nil
        }
    
    private def ifHaveDerivativeAssets(next: CssBindFunc) =
        "#ifHaveDerivativeAssets" #> { n =>
            if (!myDerivativeAssets.isEmpty) next(n)
            else Nil
        }
    
    private def ifNoDerivativeAssets =
        "#ifNoDerivativeAssets" #> { n =>
            if (myDerivativeAssets.isEmpty) n
            else Nil
        }
    
    // TODO: This is not right
    def stockVolume(stock: StockAsset): BigDecimal =
        BigDecimal("3.14")
}

object Portfolio extends RefreshHub

