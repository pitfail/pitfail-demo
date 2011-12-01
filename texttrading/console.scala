
package texttrading

// Console frontend (for testing!)

class ConsoleFrontend(
        username: String
    )
    extends Frontend
{
    def messages = scala.io.Source.stdin.getLines map { line =>
        Message(username, line, reply)
    }

    val reply = new Reply {
        def reply(text: String) = println(text)
    }
}

