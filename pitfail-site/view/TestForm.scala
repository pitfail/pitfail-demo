
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http.{js,StatefulSnippet}
import js._
import JsCmds._
import JE._
import Helpers._

import up._
import HList._
import KList._
import ~>._

import scala.math.{BigDecimal}
import lib.magicform._
import lib.formats._

class TestForm extends StatefulSnippet with Loggable {
    
    def dispatch = {
        case "render" => render
    }
    
    case class Point(
        x: BigDecimal,
        y: BigDecimal
    )
    object Point {
        def fromHList(hl: BigDecimal :+: BigDecimal :+: HNil): Point = hl match {
            case x :+: y :+: HNil => Point(x, y)
        }
    }
    
    object form extends Form[Seq[Point]](this,
        ListField(this, "points",
            AggregateField(Point.fromHList _,
                    NumberField("x", "1")
                :^: NumberField("y", "2")
                :^: KNil
            )
        )
    )
    {
        def act(s: Seq[Point]) {
            logger.info(s)
        }
    }
    
    def render = form.render
}

