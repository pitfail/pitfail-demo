
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

case class Func(help: String, name: String, body: String)

lazy val jsFuncDefs: NodeSeq =
    <head>
        <script type="text/javascript">
            {setup}
        </script>
    </head>

lazy val api = List[Func](
    Func(
    """| Buy a stock in dollars.
       |
       | buyDollars('MSFT', 2000)
       |""".stripMargin,
    "buyDollars", func("ticker", "dollars") { result =>
        val ticker  = result("ticker")
        val dollars = Dollars(result("dollars"))
        
        me.userBuyStock(ticker, dollars)
        Noop
    }),
    
    Func(
    """| Buy a stock in shares.
       |
       | buyShares('MSFT', 10)
       |""".stripMargin,
    "buyShares", func("ticker", "shares") { result =>
        val ticker = result("ticker")
        val shares = Shares(result("shares"))
        
        me.userBuyStock(ticker, shares)
        Noop
    }),
    
    Func(
    """| Sell a stock in shares.
       |
       | sellDollars('MSFT', 2000)
       |""".stripMargin,
    "sellDollars", func("ticker", "dollars") { result =>
        val ticker = result("ticker")
        val dollars = Dollars(result("dollars"))
        
        me.userSellStock(ticker, dollars)
        Noop
    }),
    
    Func(
    """| Sell a stock in shares.
       |
       | sellShares('MSFT', 10)
       |""".stripMargin,
    "sellShares", func("ticker", "shares") { result =>
        val ticker = result("ticker")
        val shares = Shares(result("shares"))
        
        me.userSellStock(ticker, shares)
        Noop
    }),
    
    Func(
    """| Sell all of the stock that you own.
       |
       | sellAll('MSFT')
       |""".stripMargin,
    "sellAll", func("ticker") { result =>
        val ticker = result("ticker")
        
        me.userSellAll(ticker)
        Noop
    }),
    
    Func(
    """| Get the current price of a stock (in a callback).
       |
       | stockPrice('MSFT', function(price) {
       |     output.append(price)
       | })
       |""".stripMargin,
    "stockPrice", func("ticker") { result =>
        val ticker = result("ticker")
        Num(Stocks.lastTradePrice(ticker).price.doubleValue)
    }),
    
    Func(
    """| Get how many shares of a stock you own.
       |
       | howManySharesDoIOwn('MSFT', function(shares) {
       |     output.append(shares)
       | })
       |""".stripMargin,
    "howManySharesDoIOwn", func("ticker") { result =>
        val ticker = result("ticker")
        Num(me.howManyShares(ticker).shares)
    }),
    
    Func(
    """| Get how many dollars (at last traded price) of a stock you own.
       |
       | howManyDollarsDoIOwn('MSFT', function(dollars) {
       |     output.append(dollars)
       | })
       |""".stripMargin,
    "howManyDollarsDoIOwn", func("ticker") { result =>
        val ticker = result("ticker")
        val howMany = me.howManyDollars(ticker).double
        
        logger.info("Replying %s %s" format (ticker, howMany))
        Num(howMany)
    }),
    
    Func(
    """| Get recent news events.
       |
       | news(function(events) {
       |     for (i in events) {
       |         output.append(events[i].action)
       |         output.append(events[i].trader)
       |         output.append(events[i].ticker)
       |         output.append(events[i].dollars)
       |         output.append(events[i].price)
       |     }
       | })
       |""".stripMargin,
    "news", func() { result => JsArray {
        recentEvents(10) flatMap { ev =>
            ev.action match {
                case Bought(buyer, stock, shares, dollars, price) =>
                    Some(JsObj("action"->"bought", "trader"->buyer.name,
                        "ticker"->stock, "shares"->shares.double,
                        "dollars"->dollars.double, "price"->price.double))
                    
                case Sold(seller, stock, shares, dollars, price) =>
                    Some(JsObj("action"->"sold", "trader"->seller.name, "ticker"->stock,
                        "shares"->shares.double, "dollars"->dollars.double,
                        "price"->price.double))
                    
                case Offered(from, to, derivative, price) => None
                case Accepted(from, to, derivative, price, _, _) => None
                case Declined(from, to, derivative, price) => None
                case Auctioned(from, derivative, price) => None
                case Bid(from, on, price) => None
                case Closed(port, offer) => None
                case Exercised(port, derivative) => None
            }
        }
    }})
)

def me = control.PortfolioSwitcher.currentPortfolio

lazy val setup = ("$(function () {\n"
    + (api map { case Func(help, name, value) =>
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
        |    fullCode = "key = '"+key+"'; output = $('#pane-'+key); {\n" + code + "\n}"
        |    try {
        |        eval(fullCode)
        |    }
        |    catch (e) {
        |        outplace.text(outplace.text() + e + "\n")
        |        throw e
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
    val argList = (args:+"callback") mkString ", "
    val (id, stuff) = SHtml.jsonCall(JsRaw("{"+catchList+"}"), opFunc _)
    
    val js = ("liftAjax.lift_ajaxHandler("
        +"'"+id+"=' + encodeURIComponent(JSON.stringify({"+catchList+"})), "
        +"function(x) callback(eval(x)), null, null)")
    
    "(function("+argList+") { "+js+"});"
}

//
}

