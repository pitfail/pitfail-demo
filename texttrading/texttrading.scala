
package texttrading

trait Backend {
    def perform(request: Request): Response
}

trait Frontend {
    def messages: Iterator[Message]
}

class TextTrader(
        backend:  Backend,
        frontend: Frontend
    )
{
    def run() {
        import parser._
        
        frontend.messages foreach { case Message(user, command, reply) =>
            val parsed = parseAction(command)
            
            def huh =
                reply("""|I don't understand your command
                         |Try:
                         |%s
                         |"""
                    .stripMargin
                    .format(commandIntro)
                )
            
            parsed match {
                case Success(action, _) =>
                    val request = Request(user, action)
                    val response = backend.perform(request)
                    
                    response.extraMsgs foreach (reply(_))
                    response.status match {
                        case OK          => reply("Success!")
                        case Failed(msg) => reply(msg)
                    }
                    
                case Failure(_, _) => huh
                case Error(_, _)   => huh
            }
        }
    }
}


