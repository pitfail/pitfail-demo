
package bootstrap.liftweb

import net.liftweb.{mapper, common, http, util}
import common.{Box,Full,Empty}
import util.{Props}
import http.{LiftRules}
import code.model.Schema

object DBSetup {
    def apply() {
        Schema.init()
    }
}

