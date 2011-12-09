
package model

import errors._
import scala.collection.mutable.ArrayBuffer
import net.liftweb.common.Loggable

import net.liftweb.actor
import actor._
import net.liftweb.http._

import scalaz._
import scalaz.Scalaz._
import spser._

trait DBMagic extends Loggable {
    self: Schema =>
    
    trait KL extends {
        val id: Key
        
        def ~~(o: KL) = o.id == this.id
        def ~~(o: Key) = o == this.id
    }
    object KL {
        implicit def toLink[R <: KL](kl: R): Link[R] = new Link(kl.id)
    }
    
    implicit val klEncode: SQLEncode[KL] = new SQLEncode[KL] {
        type CT = Key
        def encode(k: KL) = k.id
    }
    
    class Link[R <: KL](val id: Key) {
        def extract(implicit table: Table[R]) = readDB {
            (table where ('id ~=~ id)).headOption getOrElse (throw NotFound)
        }
        
        override def toString = "["+id+"]"
        
        def ~~(o: KL) = o.id == this.id
        def ~~(o: Link[R]) = o.id == this.id
    }
    object Link {
        implicit def extract[R <: KL](link: Link[R])(implicit table: Table[R]) =
            link.extract
    }
    
    implicit def sqlLink[R<:KL]: SQLType[Link[R]] = new SQLType[Link[R]] {
        type CT = String
        val typeName = "VARCHAR"
        def encode(l: Link[R]) = l.id
        def decode(k: Key) = new Link[R](k)
    }

    case object NotFound extends RuntimeException
    implicit def idToLink[R<:KL](id: Key): Link[R] = new Link[R](id)

    // Only use this inside the model code. Then make public proxy methods for
    // outside-the-model code.
    private[model] def editDB[A](trans: => Transaction[A]) = {
        inTransaction {
            val Transaction(result, ops) = trans
            ops foreach (_.perform)
        
            val tables = ops flatMap (_.affectedTables)
            tables foreach (_ ! Refresh)
            result
        }
    }
    
    def readDB[A](trans: => A) = {
        inTransaction {
            trans
        }
    }
    
    case class Transaction[+A](result: A, ops: Seq[EditOp]) {
        def flatMap[B](f: A => Transaction[B]) = {
            val next = f(result)
            Transaction(next.result, ops ++ next.ops)
        }
        
        def map[B](f: A => B) = Transaction(f(result), ops)
        def filter(f: A => Boolean) =
            if (f(result)) this
            else sys.error("Match error")
        
        def &[B](tr: Transaction[B]) = Transaction((result,tr.result), ops ++ tr.ops)
    }
    object Transaction {
        def apply[A](result: A) = new Transaction(result, Seq())
    }
    
    implicit val TransactionPure: Pure[Transaction] = new Pure[Transaction] {
        def pure[A](a: => A) = Transaction(a)
    }
    
    implicit val TransactionFunctor: Functor[Transaction] = new Functor[Transaction] {
        def fmap[A,B](t: Transaction[A], f: A => B) = t map f
    }
    
    implicit val TransactionBind: Bind[Transaction] = new Bind[Transaction] {
        def bind[A,B](a: Transaction[A], f: A => Transaction[B]) = a flatMap f
    }
    
    trait EditOp {
        def perform(): Unit
        val affectedTables: Seq[Table[_]]
    }
    
    case class Insert[R<:KL](rec: R, table: Table[R]) extends EditOp {
        def perform() = {
            table where ('id ~=~ rec.id) delete()
            table insert rec
        }
        val affectedTables = Seq(table)
    }
    
    case class InsertFor[R<:KL](rec: R, loc: Where, table: Table[R]) extends EditOp {
        def perform() = {
            table where loc delete()
            table where ('id ~=~ rec.id) delete()
            table insert rec
        }
        val affectedTables = table :: Nil
    }
    
    case class Update[R<:KL](id: Key, by: R=>R, table: Table[R]) extends EditOp {
        def perform() = {
            val old = (table where ('id ~=~ id)).headOption getOrElse (throw NotFound)
            val next = by(old)
            assert(next.id == old.id, "You changed an object's ID")
            
            table where ('id ~=~ id) set next
        }
        val affectedTables = Seq(table)
    }

    case class Delete[R<:KL](rec: R, table: Table[R]) extends EditOp {
        def perform() = {
            table where ('id ~=~ rec.id) delete()
        }
        val affectedTables = Seq(table)
    }
    
    implicit def toOps[R<:KL](rec: R) = new {
        def insert(implicit table: Table[R]) = Transaction(rec, Insert(rec, table) :: Nil)
        def insertFor(loc: Where)(implicit table: Table[R]) = Transaction(rec, InsertFor(rec, loc, table)::Nil)
        def update(by: R=>R)(implicit table: Table[R]) = Transaction((), Update(rec.id, by, table) :: Nil)
        def delete(implicit table: Table[R]) = Transaction(rec, Delete(rec, table) :: Nil)
        def refetch(implicit table: Table[R]) =
            (table where ('id ~=~ rec.id)).headOption getOrElse (throw NotFound)
    }
    
    implicit def linkToOps[R<:KL](link: Link[R]) = new {
        def update(by: R=>R)(implicit table: Table[R]) = Transaction((), Update(link.id, by, table)::Nil)
    }
    
    def mutually[A](op: (Key, Key) => A) = op(nextID, nextID)
    def mutually[A](op: (Key, Key, Key) => A) = op(nextID, nextID, nextID)
}

trait RefreshHub extends LiftActor
    with ListenerManager
    with Loggable
{
    def createUpdate = Refresh
    
    override def lowPriority = {
        case Refresh =>
            updateListeners()
    }
    
    def apply() { this ! Refresh }
}

case object Refresh

