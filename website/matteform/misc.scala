
import net.liftweb.{http, util}
import util._
import http._
import js._
import JsCmds._
import JE._
import Helpers._
import scala.xml.NodeSeq

package object matteform {
    val same: CssBindFunc = "#thisnamedoesnotexist" #> { p =>
        sys.error("This name does not exist!"): NodeSeq
    }
        
    val KNil = up.KNil
    val HNil = up.HNil
    val :+: = up.HList.:+:
    type HNil = up.HNil
    type :+:[H, T <: up.HList] = up.HList.:+:[H, T]
    
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
}

