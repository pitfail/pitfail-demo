
package bootstrap.liftweb

import code.model.Schema.{createSchema}

object SetupMain {
    
    def main(args: Array[String]) {
        createSchema()
    }
}

