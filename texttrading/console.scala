
package texttrading

// Console frontend (for testing!)

class ConsoleFrontend extends Frontend {
    
    def messages = scala.io.Source(stdin).getLines
    
}

