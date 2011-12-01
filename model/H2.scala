
package model

import org.squeryl

// H2 is the backend driver for the database.

trait H2Schema extends squeryl.Schema {
    val dbDriver = "org.h2.Driver"
    val adapter  = new squeryl.adapters.H2Adapter
    val dbUrl    = "jdbc:h2:data;AUTO_SERVER=TRUE"
    val dbFile   = "data.h2.db"
    
    // This must be called before any database operations are performed.
    // It should be called from Boot.scala
    def init() {
        import squeryl.SessionFactory
        import squeryl.Session
        
        Class.forName(dbDriver)
        SessionFactory.concreteFactory = Some(() =>
            Session.create(
                java.sql.DriverManager.getConnection(dbUrl),
                adapter
            )
        )
    }

    // Deletes the database (if it exists) and makes a new one
    def clearDatabase() {
        import squeryl.PrimitiveTypeMode._
        
        new java.io.File(dbFile).delete()
        init()
        transaction {
            this.create
        }
    }
    
    // Get the Schema as text
    def schemaDDL: String = {
        var schema: String = ""
        
        this.printDdl {st =>
            schema += st
            schema += "\n"
            schema += "\n"
        }

        schema
    }
}

