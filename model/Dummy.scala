
package model

trait DummySchema {
    // This must be called before any database operations are performed.
    // It should be called from Boot.scala
    def init() {
    }

    // Deletes the database (if it exists) and makes a new one
    def clearDatabase() {
    }
    
    // Get the Schema as text
    def schemaDDL: String = ""
}


