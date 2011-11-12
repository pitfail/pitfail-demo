
package model

import org.squeryl
import squeryl.PrimitiveTypeMode._
import squeryl.customtypes.StringField
import squeryl.{KeyedEntity,SessionFactory,Session,Table}
import squeryl.dsl._
import squeryl.dsl.ast._
import scala.util.continuations._ // magic

trait DBMagic extends Transactions with squeryl.Schema

trait Links {
    case object NotFound extends RuntimeException
    
    type Key = String
    
    trait KL extends KeyedEntity[Key] {
        val id: Key
    }
    object KL {
        implicit def toLink[R <: KL](kl: R): Link[R] = new Link(kl.id)
    }

    class Link[R <: KL](id: Key) extends StringField(id) {
        var target: Option[R] = None
        def extract(implicit table: Table[R]) = target getOrElse {
            target = table lookup id
            target getOrElse (throw NotFound)
        }
        
        override def toString = "["+id+"]"
    }
    object Link {
        implicit def extract[R <: KL](link: Link[R])(implicit table: Table[R]) =
            link.extract
    }
    implicit def idToLink[R <: KL](id: Key): Link[R] = new Link[R](id)

    def nextID: Key = java.util.UUID.randomUUID.toString.substring(0, 5)
}

trait Transactions extends Links {
    def editDB(trans: => Seq[UpdateOp] @cps[Seq[UpdateOp]]) {
        transaction { reset(trans) foreach (_.perform) }
    }
    
    def readDB[A](trans: => A): A = transaction { trans }
    
    implicit def toOrCreate[A](result: => A) = new {
        def orCreate(make: => (A, Seq[UpdateOp])) = shift { (cont: A=>Seq[UpdateOp]) =>
            try cont(result)
            catch { case _: RuntimeException =>
                val (a, ops) = make
                ops ++ cont(a)
            }
        }
    }
    
    sealed trait UpdateOp {
        def perform(): Unit
    }
    
    val Done = Seq[UpdateOp]()
    
    implicit def toOperations[R <: KL](r: R)(t: Table[R]) = new {
        def insert() = Seq[UpdateOp](new UpdateOp {
            def perform() {
                println("Inserting " + r)
                t insert r
            }
        })
        
        def update() = Seq[UpdateOp](new UpdateOp {
            def perform() {
                println("Updating " + r)
                t update r
            }
        })
    }
    
    implicit def seqAnd[A](s: Seq[A]) = new {
        def &[B >: A](s2: Seq[B]) = s ++ s2
    }
    
    def mutually[A](op: (Key, Key) => A) = op(nextID, nextID)
}


