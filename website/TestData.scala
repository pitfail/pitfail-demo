
package bootstrap.liftweb

import code._

import model._
import model.schema._

// Joda time
import org.joda.time.DateTime

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
    
    systemRecalculateRankings()
}

}

