
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
    
    // Visible error messages for the user
    def liftMsg(id: String) =
        <lift:Msg id={id} errorClass="inputError"/>
}

