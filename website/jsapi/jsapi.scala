
import net.liftweb.{common, http}
import common.Loggable

package object jsapi extends Loggable {
//

import scala.xml._

import code._
import control.LoginManager._

import model._
import model.schema._

import http._
import http.js._
import JsCmds._
import JE._

lazy val jsFuncDefs: NodeSeq =
    <head>
        <script type="text/javascript">
            {setup}
        </script>
    </head>

lazy val api = Map(
    "buyDollars" -> func("ticker", "dollars") { result =>
        val ticker  = result("ticker")
        val dollars = Dollars(result("dollars"))
        
        me.userBuyStock(ticker, dollars)
        Noop
    },
    
    "buyShares" -> func("ticker", "shares") { result =>
        val ticker = result("ticker")
        val shares = Shares(result("shares"))
        
        me.userBuyStock(ticker, shares)
        Noop
    },
    
    "sellDollars" -> func("ticker", "dollars") { result =>
        val ticker = result("ticker")
        val dollars = Dollars(result("dollars"))
        
        me.userSellStock(ticker, dollars)
        Noop
    },
    
    "sellShares" -> func("ticker", "shares") { result =>
        val ticker = result("ticker")
        val shares = Shares(result("shares"))
        
        me.userSellStock(ticker, shares)
        Noop
    },
    
    "sellAll" -> func("ticker") { result =>
        val ticker = result("ticker")
        
        me.userSellAll(ticker)
        Noop
    },
    
    "stockPrice" -> func("ticker") { result =>
        val ticker = result("ticker")
        Num(Stocks.stockPrice(ticker).price.doubleValue)
    },
    
    "howManySharesDoIOwn" -> func("ticker") { result =>
        val ticker = result("ticker")
        Num(me.howManyShares(ticker).shares)
    },
    
    "howManyDollarsDoIOwn" -> func("ticker") { result =>
        val ticker = result("ticker")
        Num(me.howManyDollars(ticker).dollars)
    }
)

def me = control.PortfolioSwitcher.currentPortfolio

lazy val setup = ("$(function () {\n"
    + (api map { case (name, value) =>
        "    self."+name+" = " + value;
    } mkString ";\n")
    + otherLibs
    + "\n} )"
)

lazy val otherLibs =
    """|
       | self.runAutoTrade = (function(key) {
       |    var code = $("#code-"+key).val()
       |    var outplace = $("#output-"+key)
       |    outplace.text("")
       |    try {
       |        eval(code)
       |    }
       |    catch (e) {
       |        outplace.text(outplace.text() + e + "\n")
       |    }
       | })
       |
       |""".stripMargin

def func(args: String*)(callback: Map[String,String]=>JsCmd) = {
    def opFunc(arg: Any) = {
        logger.info("Got AutoTrade " + (arg.asInstanceOf[AnyRef]).toString)
        
        try
            callback(arg.asInstanceOf[Map[String,String]])
        catch {
            case _: ClassCastException =>
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

//
}

