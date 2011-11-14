
package model

import errors._
import scala.collection.mutable.ArrayBuffer
import net.liftweb.common.Loggable

// This is very very temporary and should be replaced soon!
class Table[R <: KL] extends ArrayBuffer[R] with Loggable {
    def lookup(k: Key) = {
        this filter (_.id ==  k) headOption
    }
    def insert(r: R) { this += r }
    def update(r: R) {
        this remove (this indexWhere (_.id == r.id))
        this insert r
    }
    def delete(r: R) { this remove (this indexWhere (_.id == r.id)) }
}
    
trait DBMagic extends Transactions{
    def table[R <: KL] = new Table[R]()
}

trait KL extends {
    val id: Key
    
    def ~~(o: KL) = o.id == this.id
    def ~~(o: Key) = o == this.id
}
object KL {
    implicit def toLink[R <: KL](kl: R): Link[R] = new Link(kl.id)
}

class Link[R <: KL](val id: Key) {
    var target: Option[R] = None
    def extract(implicit table: Table[R]) = target getOrElse {
        // TODO: this
        //target = table lookup id
        //target getOrElse (throw NotFound)
        table lookup id getOrElse (throw NotFound)
    }
    
    override def toString = "["+id+"]"
    
    def ~~(o: KL) = o.id == this.id
    def ~~(o: Link[R]) = o.id == this.id
}
object Link {
    implicit def extract[R <: KL](link: Link[R])(implicit table: Table[R]) =
        link.extract
}

trait Links {
    case object NotFound extends RuntimeException
    implicit def idToLink[R<:KL](id: Key): Link[R] = new Link[R](id)
}

trait Transactions extends Links with Loggable {
    
    def editDB[A](trans: => Transaction[A]) = {
        val Transaction(result, ops) = trans
        ops foreach (_.perform)
        result
    }
    
    def readDB[A](trans: => A) = trans
    
    case class Transaction[+A](result: A, ops: Seq[EditOp]) {
        def flatMap[B](f: A => Transaction[B]) = {
            val next = f(result)
            Transaction(next.result, ops ++ next.ops)
        }
        
        def map[B](f: A => B) = Transaction(f(result), ops)
        
        def &[B](tr: Transaction[B]) = Transaction((result,tr.result), ops ++ tr.ops)
    }
    object Transaction {
        def apply[A](result: A) = new Transaction(result, Seq())
    }
    
    sealed trait EditOp {
        def perform(): Unit
    }
    
    case class Insert[R<:KL](rec: R, table: Table[R]) extends EditOp {
        def perform() = {
            table.insert(rec)
        }
    }
    
    case class Update[R<:KL](rec: R, table: Table[R]) extends EditOp {
        def perform() = {
            table.update(rec)
        }
    }
    
    case class Delete[R<:KL](rec: R, table: Table[R]) extends EditOp {
        def perform() = {
            table.delete(rec)
        }
    }
    
    implicit def toOps[R<:KL](rec: R) = new {
        def insert(implicit table: Table[R]) = Transaction(rec, Insert(rec, table) :: Nil)
        def update(implicit table: Table[R]) = Transaction(rec, Update(rec, table) :: Nil)
        def delete(implicit table: Table[R]) = Transaction(rec, Delete(rec, table) :: Nil)
    }
    
    implicit def toOrCreate[R](already: => R) = new {
        def orCreate(trans: => Transaction[R]) =
            try {
                Transaction(already, Nil)
            }
            catch { case _: BadUser =>
                trans
            }
    }
    
    def mutually[A](op: (Key, Key) => A) = op(nextID, nextID)
}

