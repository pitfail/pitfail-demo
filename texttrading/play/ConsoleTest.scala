
package texttrading
package test

object ConsoleTest {
    
    def main(args: Array[String]) {
        // Get the database running
        code.model.Schema.init()
        
        val backend  = new PitFailBackend()
        val frontend = new ConsoleFrontend("ellbur_k_a")
        
        val trader = new TextTrader(backend, frontend)
        trader.run()
    }
    
}

