
package model

import scalaz.Scalaz._

trait RankingSchema {
    self: DBMagic with UserSchema =>
    
    // Run this periodically to update everyone's standings
    def systemRecalculateRankings() = editDB(recalculateRankings)
    
    def recalculateRankings = {
        val ports = portfolios.toList sortBy (- _.spotValue)
        val ranks = List.range(1, ports.length+1)
        
        val updates: List[Transaction[Unit]] = {
            ports zip ranks map { case (port, rank) =>
                port update (p => p copy (rank=rank))
            }
        }
        
        updates.sequence
    }
}

