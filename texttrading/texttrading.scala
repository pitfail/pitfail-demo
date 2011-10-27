
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
        frontend.messages foreach { case Message(user, command, reply) =>
            val replies = TextTrader.runCommand(user, command, backend)
            replies foreach reply.reply _
        }
    }
}

object TextTrader {

    // This is pretty independent. Might as well let you call it standalone.
    def runCommand(
            user:    String,
            command: String,
            backend: Backend
        ): Seq[String] =
    {
        import parser._

        val parsed = parseAction(command)

        val huh =
            """|I don't understand your command
               |Try:
               |%s
               |"""
            .stripMargin
            .format(commandIntro)

        parsed match {
            case Success(action, _) =>
                val request = Request(user, action)
                val response = backend.perform(request)

                response.extraMsgs :+
                {
                    response.status match {
                        case OK          => "Success in " + action
                        case StringResponse(str) => str
                        case TransactionResponse(sa) => "need a bunch of magic."
                        case Failed(msg) => msg
                    }
                }

            case Failure(_, _) => Seq(huh)
            case Error(_, _)   => Seq(huh)
        }
    }
}

