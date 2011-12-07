
package bootstrap.liftweb

object DBSetup {
    def apply() {
        model.schema.createIfNecessary
    }
}

