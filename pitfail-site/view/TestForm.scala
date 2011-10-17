
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util._
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import matteform._
import scalaz.Scalaz._

class TestForm extends RefreshableSnippet with Loggable {
    
    var count: Int = 0
    
    def render(p: RefreshPoint)(in: NodeSeq) = (
           in
        |> inc.render(p) _
        |> dec.render(p) _
        |> renderCount
    )
    
    object inc extends Form[Unit](
        ConstField(()),
        formID = Some("inc")
    )
    {
        def act(u: Unit) {
            count += 1
        }
    }
    
    object dec extends Form[Unit](
        ConstField(()),
        formID = Some("dec")
    )
    {
        def act(u: Unit) {
            count -= 1
        }
    }
    
    def renderCount = "#count *" #> count
}

