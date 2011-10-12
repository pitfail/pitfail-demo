
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
import Schema.{DerivativeOffer}

import LoginManager.{currentLogin}
import org.squeryl.PrimitiveTypeMode.inTransaction

class Offers extends Refreshable with Loggable
{
    def registerWith = Offers
    
    // Things to show
    var myOffers: Seq[DerivativeOffer] = Nil
    
    override def refresh(): Unit = inTransaction {
        import Schema._
        
        for {
            name <- currentLogin
            user <- byUsername(name)
        } {
            val port = user.mainPortfolio
            
            myOffers = port.myOffers
        }
    }
    refresh()
    
    def render = (
          ifHaveOffers
        & offer
    )
    
    private def ifHaveOffers = "#ifHaveOffers" #> { p =>
        if (myOffers.isEmpty) Nil
        else p
    }
    
    private def offer = "#offer" #> (
        myOffers map ( o =>
              "user= [user]" #> o.from.owner.username
            & "#description" #> o.derivative.toHumanString
        )
    )
}

object Offers extends RefreshHub


