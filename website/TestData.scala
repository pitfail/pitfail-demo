
package bootstrap.liftweb

import code._
import model._
import model.schema._
import stockdata._

// Joda time
import org.joda.time.DateTime
import net.liftweb.json
import scalaz.Scalaz._
import scala.io.Source

object insertTestData {
//

def apply() {
    val ellburU = (User userEnsure "ellbur_k_a")
    val pitfailU = (User userEnsure "pitfail")
    
    val ellbur = ellburU.lastPortfolio
    val pitfail = pitfailU.lastPortfolio
    
    val deriv = Derivative(SecStock("MSFT", Shares(30))::Nil,
        DateTime.now.plusDays(7), CondAlways, true)
    
    val offer = pitfail.userOfferDerivativeTo(ellbur, deriv, Dollars(1000))
    
    val event = ellbur.userAcceptOffer(offer.id)
    val Some((b, s)) = event.asVotable
    
    ellbur.userVoteUp(event, b)
    
    val joejoe = pitfailU.userCreatePortfolio("joejoe")
    joejoe.userInviteUser(ellburU)
    
    def slurpResource(name: String) = {
        val resource = getClass.getClassLoader getResourceAsStream name
        if (resource == null) {
            sys.error("Failed to find " + name)
        }
        else Source fromInputStream resource mkString
    }
    
    implicit val _ = json.DefaultFormats
    val demoScripts = (
           "jsapi/demo-scripts/list.json" |> slurpResource _
        |> json.parse _ ).extract[List[String]]
        
    demoScripts foreach { script =>
        val code = slurpResource("jsapi/demo-scripts/" + script)
        
        val trade = ellbur.userMakeNewAutoTrade()
        trade.userModify(title=script, code=code)
    }
    
    pitfail.userBuyStock("MSFT", Shares(10))
    pitfail.userBuyStock("F", Shares(10))
    pitfail.userSellStock("F", Shares(2))
    
    val now = new DateTime
    model.Stocks.syntheticDividends ++= List(
        Dividend("MSFT", now plusMinutes 1, Price("0.30"))
    )
    
    systemRecalculateRankings()
    systemCheckForDividends()
}

}

