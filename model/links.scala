
package model

import org.squeryl
import squeryl.{KeyedEntity,
    SessionFactory, Session, Table}
import squeryl.customtypes.{LongField}
import squeryl.dsl._
import squeryl.dsl.ast._
import squeryl.PrimitiveTypeMode._

package object links {
//

trait KL extends KeyedEntity[Long] {
}
object KL {
    implicit def associateEntity[R <: KL](entity: R)(implicit table: Table[R]) =
        AssociatedEntity(entity, table)
}

case class Link[R <: KL](val id: Long) extends LongField(id) {
    def fetch(implicit table: Table[R]): R = inTransaction {
        from(table){t => where(t.id === this.id) select(t)} head
    }
}
object Link {
    implicit def linkToRef[R <: KL]
        (link: Link[R])(implicit table: Table[R]): R = link.fetch

    implicit def linkToID[R <: KL] (link: Link[R]): Long = link.id

    implicit def linkToNumExp[R <: KL] (link: Link[R]): NumericalExpression[_]
        = if (link == null) 0 else link.id.~
    
    implicit def linkToAssociated[R <: KL](link: Link[R])(implicit table: Table[R]):
        AssociatedEntity[R] = KL.associateEntity(linkToRef(link))
}

implicit def idToLink[R <: KL] (id: Long): Link[R] = Link[R](id)
implicit def refToLink[R <: KL] (ref: R): Link[R] = Link[R](ref.id)
implicit def refToNumExp(kl: KL): NumericalExpression[_] = kl.id
    
case class AssociatedEntity[R <: KL](
        entity: R,
        table: Table[R]
    )
{
    def delete(): Unit = inTransaction {
        table.deleteWhere(r => r.id === entity.id)
    }
    
    def insert(): Unit = inTransaction {
        table.insert(entity)
    }
    
    def update(): Unit = inTransaction {
        table.update(entity)
    }
    
    def refetch(): Option[R] = inTransaction {
        from(table)( r =>
            where(
                r.id === entity.id
            )
            select(r)
        ) headOption
    }
}

//
}

