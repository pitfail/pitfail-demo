
package bootstrap.liftweb

import net.liftweb.common.Loggable
import code._
import model._
import model.schema._
import stockdata._

// Joda time
import org.joda.time.DateTime
import net.liftweb.json
import scalaz.Scalaz._
import scala.io.Source
import scala.util.Random

object insertTestData extends Loggable {
//

def apply() {
    readDB {
        create_!
    }
    
    val ellburU = (User userEnsure "ellbur_k_a")
    val pitfailU = (User userEnsure "pitfail")
    
    val ellbur = ellburU.lastPortfolio
    val pitfail = pitfailU.lastPortfolio
    
    val deriv = Derivative(SecStock("MSFT", Shares(30))::Nil,
        DateTime.now.plusDays(7), CondAlways(), true)
    
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
    
    // Synthetic history
    readDB { portfolios.toList map { port =>
        logger.info("Making synthetic data for " + port.name)
        val historyStart = now minusWeeks 1 minusDays 1
        var mark = now
        var value = port.spotValue
        while (mark isAfter historyStart) {
            PeriodicPortfolioEvaluator makeSynthetic (port, mark, value)
            mark = mark minusHours 6
            
            val scale = Scale(1 + Random.nextGaussian/10)
            logger.info(" scale = " + scale)
            value = value * scale
        }
    }}
    
    systemRecalculateRankings()
    systemCheckForDividends()
}

}

