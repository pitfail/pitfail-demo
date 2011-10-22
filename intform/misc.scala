
import up._
import HList._
import KList._
import ~>._
import scala.xml.NodeSeq

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
    
    // -----------------------------------------------------------------
    
    private type FLA = FieldListAdd
    private val flz = FieldList.empty
    
    implicit def fl1[T1<:FLA](t:T1) = t.a(flz)
    implicit def fl2[T1<:FLA,T2<:FLA](t: (T1,T2)) = t._1.a(t._2.a(flz))
    implicit def fl3[T1<:FLA,T2<:FLA,T3<:FLA](t: (T1,T2,T3)) = t._1.a(t._2.a(t._3.a(flz)))
    implicit def fl4[T1<:FLA,T2<:FLA,T3<:FLA,T4<:FLA](t: (T1,T2,T3,T4)) =
        t._1.a(t._2.a(t._3.a(t._4.a(flz))))
    implicit def fl5[T1<:FLA,T2<:FLA,T3<:FLA,T4<:FLA,T5<:FLA](t: (T1,T2,T3,T4,T5)) =
        t._1.a(t._2.a(t._3.a(t._4.a(t._5.a(flz)))))
    
    // -----------------------------------------------------------------
    
    implicit def tuplist1[A](a: A) = Seq[A](a)
    implicit def tuplist2[A](a: (A,A)) = Seq[A](a._1, a._2)
    implicit def tuplist3[A](a: (A,A,A)) = Seq[A](a._1, a._2, a._3)
    implicit def tuplist4[A](a: (A,A,A,A)) = Seq[A](a._1, a._2, a._3, a._4)
    implicit def tuplist5[A](a: (A,A,A,A,A)) = Seq[A](a._1, a._2, a._3, a._4)
    
    // -----------------------------------------------------------------
    
    private type FR = FieldRender
    private type NS = NodeSeq
    
    def F[A <% NS](m: (FR) => A): (FR:+:HNil) => NS = {
        case t1:+:_ => m(t1)
    }

    def F[A <% NS](m: (FR,FR) => A): (FR:+:FR:+:HNil) => NS = {
        case t1:+:t2:+:_ => m(t1,t2)
    }
    
    def F[A <% NS](m: (FR,FR,FR) => A): (FR:+:FR:+:FR:+:HNil) => NS = {
        case t1:+:t2:+:t3:+:_ => m(t1,t2,t3)
    }
    
    def F[A <% NS](m: (FR,FR,FR,FR) => A): (FR:+:FR:+:FR:+:FR:+:HNil) => NS = {
        case t1:+:t2:+:t3:+:t4:+:_ => m(t1,t2,t3,t4)
    }
    
    def F[A <% NS](m: (FR,FR,FR,FR,FR) => A): (FR:+:FR:+:FR:+:FR:+:FR:+:HNil) => NS = {
        case t1:+:t2:+:t3:+:t4:+:t5:+:_ => m(t1,t2,t3,t4,t5)
    }
    
    // -----------------------------------------------------------------
    
    def wc = throw new IllegalStateException("Wrong number of cases")
    
    private type CR = ChoiceRender
    
    def C[A <% NS](m: (CR) => A): List[CR] => A = {
        case t1::Nil => m(t1)
        case _ => wc
    }

    def C[A <% NS](m: (CR,CR) => A): List[CR] => A = {
        case t1::t2::Nil => m(t1,t2)
        case _ => wc
    }
    
    def C[A <% NS](m: (CR,CR,CR) => A): List[CR] => A = {
        case t1::t2::t3::Nil => m(t1,t2,t3)
        case _ => wc
    }
    
    def C[A <% NS](m: (CR,CR,CR,CR) => A): List[CR] => A = {
        case t1::t2::t3::t4::Nil => m(t1,t2,t3,t4)
        case _ => wc
    }
    
    def C[A <% NS](m: (CR,CR,CR,CR,CR) => A): List[CR] => A = {
        case t1::t2::t3::t4::t5::Nil => m(t1,t2,t3,t4,t5)
        case _ => wc
    }
}

