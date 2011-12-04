
package model

trait AutoTradeSchema {
    self: DBMagic with UserSchema =>
    
    implicit val autoTrades = table[AutoTrade]
    
    case class AutoTrade(
            id:    Key = nextID,
            owner: Portfolio,
            title: String,
            code:  String
        )
        extends KL
        with AutoTradeOps
    
    trait PortfolioWithAutoTrades {
        self: Portfolio =>
    
        def userMakeNewAutoTrade() = editDB(makeNewAutoTrade)
        
        def myAutoTrades = autoTrades filter (_.owner~~this) toList
        
        private[model] def makeNewAutoTrade =
            AutoTrade(owner=this, title="", code="").insert
    }
    
    trait AutoTradeOps {
        self: AutoTrade =>
        
        def userModify(title: String, code: String) =
            editDB(modify(title, code))
        
        def userDelete() = editDB(this.delete)
        
        private[model] def modify(title: String, code: String) =
            this update (t => t copy (title=title, code=code))
    }
}

