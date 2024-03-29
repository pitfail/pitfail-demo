
// Written by: Owen Healy
// Written by: Michael Koval

import up._
import HList._
import KList._
import ~>._
import scala.xml._

package object intform {
    
    implicit def hlistify1[T1,A](m: (T1) => A): (T1:+:HNil) => A = {
        case t1:+:HNil => m(t1)
    }

    implicit def hlistify2[T1,T2,A](m: (T1,T2) => A): (T1:+:T2:+:HNil) => A = {
        case t1:+:t2:+:HNil => m(t1, t2)
    }
    
    implicit def hlistify3[T1,T2,T3,A](m: (T1,T2,T3) => A): (T1:+:T2:+:T3:+:HNil) => A = {
        case t1:+:t2:+:t3:+:HNil => m(t1, t2, t3)
    }
    
    implicit def hlistify4[T1,T2,T3,T4,A](m: (T1,T2,T3,T4) => A): (T1:+:T2:+:T3:+:T4:+:HNil) => A = {
        case t1:+:t2:+:t3:+:t4:+:HNil => m(t1, t2, t3, t4)
    }
    
    implicit def hlistify5[T1,T2,T3,T4,T5,A](m: (T1,T2,T3,T4,T5) => A): (T1:+:T2:+:T3:+:T4:+:T5:+:HNil) => A = {
        case t1:+:t2:+:t3:+:t4:+:t5:+:HNil => m(t1, t2, t3, t4, t5)
    }
    
    implicit def hlistify6[T1,T2,T3,T4,T5,T6,A](m: (T1,T2,T3,T4,T5,T6) => A): (T1:+:T2:+:T3:+:T4:+:T5:+:T6:+:HNil) => A = {
        case t1:+:t2:+:t3:+:t4:+:t5:+:t6:+:HNil => m(t1, t2, t3, t4, t5, t6)
    }
    
    implicit def hlistify7[T1,T2,T3,T4,T5,T6,T7,A](m: (T1,T2,T3,T4,T5,T6,T7) => A):
        (T1:+:T2:+:T3:+:T4:+:T5:+:T6:+:T7:+:HNil) => A =
    {
        case t1:+:t2:+:t3:+:t4:+:t5:+:t6:+:t7:+:HNil => m(t1, t2, t3, t4, t5, t6, t7)
    }
    
    implicit def hlistify8[T1,T2,T3,T4,T5,T6,T7,T8,A](m: (T1,T2,T3,T4,T5,T6,T7,T8) => A):
        (T1:+:T2:+:T3:+:T4:+:T5:+:T6:+:T7:+:T8:+:HNil) => A =
    {
        case t1:+:t2:+:t3:+:t4:+:t5:+:t6:+:t7:+:t8:+:HNil => m(t1, t2, t3, t4, t5, t6, t7, t8)
    }
    
    // -----------------------------------------------------------------
    
    implicit def tuplist1[A](a: A) = Seq[A](a)
    implicit def tuplist2[A](a: (A,A)) = Seq[A](a._1, a._2)
    implicit def tuplist3[A](a: (A,A,A)) = Seq[A](a._1, a._2, a._3)
    implicit def tuplist4[A](a: (A,A,A,A)) = Seq[A](a._1, a._2, a._3, a._4)
    implicit def tuplist5[A](a: (A,A,A,A,A)) = Seq[A](a._1, a._2, a._3, a._4)
    
    // -----------------------------------------------------------------
    
    implicit def klist1[K[+_],A](a: K[A]): KList[K,A:+:HNil] = a :^: KNil
    implicit def klist2[K[+_],A,B](t: (K[A],K[B])): KList[K,A:+:B:+:HNil] = t._1:^:t._2:^:KNil
    implicit def klist3[K[+_],A,B,C](t: (K[A],K[B],K[C])): KList[K,A:+:B:+:C:+:HNil] =
        t._1:^:t._2:^:t._3:^:KNil
    implicit def klist4[K[+_],A,B,C,D](t: (K[A],K[B],K[C],K[D])): KList[K,A:+:B:+:C:+:D:+:HNil] =
        t._1:^:t._2:^:t._3:^:t._4:^:KNil
    implicit def klist5[K[+_],A,B,C,D,E](t: (K[A],K[B],K[C],K[D],K[E])): KList[K,A:+:B:+:C:+:D:+:E:+:HNil] =
        t._1:^:t._2:^:t._3:^:t._4:^:t._5:^:KNil
    implicit def klist6[K[+_],A,B,C,D,E,F](t: (K[A],K[B],K[C],K[D],K[E],K[F])): KList[K,A:+:B:+:C:+:D:+:E:+:F:+:HNil] =
        t._1:^:t._2:^:t._3:^:t._4:^:t._5:^:t._6:^:KNil
    implicit def klist7[K[+_],A,B,C,D,E,F,G](t: (K[A],K[B],K[C],K[D],K[E],K[F],K[G])):
        KList[K,A:+:B:+:C:+:D:+:E:+:F:+:G:+:HNil] =
        t._1:^:t._2:^:t._3:^:t._4:^:t._5:^:t._6:^:t._7:^:KNil
    implicit def klist8[K[+_],A,B,C,D,E,F,G,H](t: (K[A],K[B],K[C],K[D],K[E],K[F],K[G],K[H])):
        KList[K,A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:HNil] =
        t._1:^:t._2:^:t._3:^:t._4:^:t._5:^:t._6:^:t._7:^:t._8:^:KNil
    
    // -----------------------------------------------------------------
    
    implicit def nodeSeqPlus(n: Seq[Node]): NodeSeqPlus = NodeSeqPlus(n)
    implicit def nodeSeqPlus(n: NodeSeq): NodeSeqPlus = NodeSeqPlus(n)
    implicit def nodePlus(n: Node): NodePlus = NodePlus(n)
    
    case class NodeSeqPlus(n: NodeSeq) {
        def leafMap(f: Elem => Elem): NodeSeq = n map (_ leafMap f)
    }
    
    case class NodePlus(n: Node) {
        def leafMap(f: Elem => Elem): Node = n match {
            case e: Elem =>
                f(Elem(
                    prefix     = e.prefix,
                    label      = e.label,
                    attributes = e.attributes,
                    scope      = e.scope,
                    child      = e.child leafMap f :_*
                ))
            case x@_ => x
        }
    }
    
    class MergeAttr(n1: NodeSeq) {
        def &(n2: Node) = {
            n1 leafMap { e =>
                if (e.label == n2.label)
                    e % n2.head.attributes
                else
                    e
            }
        }
    }
    implicit def mergeAttr(n1: NodeSeq) = new MergeAttr(n1)
}

