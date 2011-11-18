
package model

trait VotingSchema {
    self: UserSchema with NewsSchema =>
    
    val votingFraction = Scale("0.01")
    
    implicit val buyStockVotes = table[BuyStockVote]
        
    case class BuyStockVote(
            id:     Key = nextID,
            caster: Link[User],
            event:  Link[NewsEvent],
            asset:  Link[StockAsset]
        )
    
    trait UserWithVotes {
        self: User =>
        
        private[model] def castBuyStockVote
                (ev: NewsEvent, ticker: String, shares: Shares) =
            for {
                StockPurchase(asset=asset) <- votingPortfolio.buyStock(
                        ticker, shares * votingFraction)
                vote <- BuyStockVote(caster=this, event=ev, asset=asset).insert
            }
            yield vote
    }
}

