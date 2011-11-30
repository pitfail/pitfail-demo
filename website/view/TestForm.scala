
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
import scala.math._
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import intform._

import model._
import model.schema._

import control.LoginManager

class TestForm extends Page with Loggable {
    
    def render = {
        val (handle, js) = SHtml.jsonCall(JsRaw("{'x': 100, 'y': 50}"), { result =>
            logger.info("Received " + result)
        })
    
        def func(args: String*)(callback: Map[String,String]=>JsCmd) = {
            def opFunc(arg: Any) = {
                arg match {
                    case arg: Map[String,String] =>
                        callback(arg)
                    case _ =>
                        logger.error("Got bad data from Ajaxy stuff " + arg)
                        Noop
                }
            }
            
            val catchList = args map (name => "'"+name+"':"+name+"+''") mkString ","
            val argList = (args mkString ", ") + ", callback"
            val (id, stuff) = SHtml.jsonCall(JsRaw("{"+catchList+"}"), opFunc _)
            
            val js = ("liftAjax.lift_ajaxHandler("
                +"'"+id+"=' + encodeURIComponent(JSON.stringify({"+catchList+"})), "
                +"function(x) callback(eval(x)), null, null)")
            
            "(function("+argList+") { "+js+"});"
        }
        
        val api = Map(
            "buyDollars" -> func("ticker", "dollars") { result =>
                val ticker  = result("ticker")
                val dollars = Dollars(result("dollars"))
                
                LoginManager.currentUser.mainPortfolio.userBuyStock(ticker, dollars)
                Noop
            },
            "buyShares" -> func("ticker", "shares") { result =>
                val ticker = result("ticker")
                val shares = Shares(result("shares"))
                
                LoginManager.currentUser.mainPortfolio.userBuyStock(ticker, shares)
                Noop
            },
            "stockPrice" -> func("ticker") { result =>
                val ticker = result("ticker")
                JsRaw(Stocks.stockPrice(ticker).price.toString)
            }
        )
    
        val setup = ("$(function () {\n"
            + (api map { case (name, value) =>
                "    self."+name+" = " + value;
            } mkString ";\n")
            + "\n} )"
        )
        
        lazy val runForm: Form[String] = Form(identity[String],
            (
                commandField
            ),
            <p>Run command {commandField.main} {runSubmit.main}
                {commandField.errors} {runSubmit.errors}</p>
        )
        
        lazy val commandField = StringField("buyShares('MSFT', 100)");
        
        lazy val runSubmit = Submit(runForm, "Run") { command =>
            Run(command)
        }
        
        <head>
            <script type="text/javascript">
                {setup}
            </script>
        </head>
        <div>
        {runForm.render}
        </div>
    }
}

