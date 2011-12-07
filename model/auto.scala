
package model

import spser._

trait AutoTradeSchema extends Schema {
    self: DBMagic with UserSchema =>
    
    implicit val atCon = AutoTrade.apply _
        
    implicit val autoTrades: Table[AutoTrade] = table[AutoTrade]
    
    abstract override def tables = autoTrades :: super.tables
    
    case class AutoTrade(
            id:    Key = nextID,
            owner: Link[Portfolio],
            title: String,
            code:  String
        )
        extends KL
        with AutoTradeOps
    
    trait PortfolioWithAutoTrades {
        self: Portfolio =>
    
        def userMakeNewAutoTrade() = editDB(makeNewAutoTrade)
        
        def myAutoTrades = autoTrades where ('owner ~=~ this) toList
        
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

