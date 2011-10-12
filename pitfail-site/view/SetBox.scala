
package code
package snippet

import net.liftweb.{common, http, openid}
import common._
import http.{js, SHtml}
import js.{JsCmds, JE}
import JsCmds._
import JE._

object SetBox extends Loggable {
    import comet.TheBoxServer

    def render = SHtml onSubmit (s => {
        logger.debug("line submitted")
        TheBoxServer ! s
        SetValById("setbox_in", "")
    })
}

