
package bootstrap.liftweb

import code._

import model._
import model.schema._

// Joda time
import org.joda.time.DateTime

object insertTestData {
//

def apply() {
    val ellbur = User userEnsure "ellbur_k_a"
    val pitfail = User userEnsure "pitfail"
    
    val deriv = Derivative(SecStock("MSFT", Shares(30))::Nil,
        DateTime.now.plusDays(7), CondAlways, true)
    
    val offer = pitfail.mainPortfolio.userOfferDerivativeTo(ellbur, deriv, Dollars(1000))
    
    val event = ellbur.mainPortfolio.userAcceptOffer(offer.id)
    val Some((b, s)) = event.asVotable
    
    ellbur.mainPortfolio.userVoteUp(event, b)
    
    systemRecalculateRankings()
}

}

