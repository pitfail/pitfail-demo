
// Written by: Owen Healy

package model

// Joda time
import org.joda.time.DateTime
import scalaz.Scalaz._
import spser._

trait DividendSchema extends Schema {
    schema: DBMagic with UserSchema with StockSchema
        with SchemaErrors with NewsSchema =>
    
    implicit val dpCon = DividendPayment.apply _
            
    implicit val dividendPayments: Table[DividendPayment] = table[DividendPayment]
    
    abstract override def tables = dividendPayments :: super.tables
            
    // Record when dividends are payed out so that we
    // can inform the user
    case class DividendPayment(
            id: Key = nextID,
            owner:   Link[Portfolio],
            ticker:  String,
            dollars: Dollars,
            date:    DateTime
        )
        extends KL
    
    // ref_789  
    def systemCheckForDividends() = editDB(checkForDividends)
        
    private[model] def checkForDividends =
        {
            val now = new DateTime
            
            stockAssets.toList map { asset =>
                val dividends = Stocks.recentDividends(asset.ticker)
                val newDividends = dividends filter ( d =>
                       (d.date isAfter asset.lastDividendDate)
                    && (d.date isBefore now)
                )
                
                val totalPrice = Price(newDividends map (_.price.price) sum)
                val dollars = totalPrice * asset.shares
                
                for {
                    _ <- asset update (a => a copy (
                        lastDividendDate = new DateTime,
                        totalDividends = a.totalDividends + dollars
                    ))
                    _ <- asset.owner.extract update (p => p copy (cash=p.cash+dollars))
                    _ <- DividendPayment(ticker=asset.ticker, owner=asset.owner,
                            dollars=dollars, date=now).insert
                }
                yield ()
            }
        }.sequence
    
    trait PortfolioWithDividends {
        self: Portfolio =>
        
        // ref_489
        def myDividendPayments: List[DividendPayment] =
            dividendPayments where ('owner ~=~ this) toList
    }
}

