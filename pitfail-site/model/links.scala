
package code
package model

import org.squeryl
import squeryl.{KeyedEntity,
    SessionFactory, Session, Table}
import squeryl.customtypes.{LongField}
import squeryl.dsl._
import squeryl.dsl.ast._
import squeryl.PrimitiveTypeMode._

object links {
    object Sugar {
        type KL = KeyedEntity[Long]
    
        case class Link[R <: KeyedEntity[Long]](val id: Long)
            extends LongField(id)

        implicit def linkToRef[R <: KL]
                    (link: Link[R])(implicit table: Table[R]): R
            = inTransaction {
                from(table){t => where(t.id === link.id) select(t)} head
            }

        implicit def linkToID[R <: KL]
                    (link: Link[R]): Long
            = link.id
        
        implicit def linkToNumExp[R <: KL]
                    (link: Link[R]): NumericalExpression[_]
            = if (link == null) 0 else link.id.~

        implicit def idToLink[R <: KL]
                    (id: Long): Link[R]
            = Link[R](id)
                
        implicit def refToLink[R <: KL]
                    (ref: R): Link[R]
            = Link[R](ref.id)
        
        implicit def refToNumExp(kl: KL): NumericalExpression[_]
            = kl.id
    }
}


