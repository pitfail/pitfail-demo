
package code
package control

import net.liftweb.{common, http, openid, util, json}
import common._
import util._
import http._

object Logout {

    def dispatchPF: LiftRules.DispatchPF =
        NamedPF("Twitter login callback") {
            case req@ Req("logout"::Nil,    "", _) => userLogout(req) _
        }

    def userLogout(req: Req)(): Box[LiftResponse] = {
        LoginManager.logout()
        Full(RedirectResponse(S.referer openOr "/"))
    }
}

