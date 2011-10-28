
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
                        case OK          => action match {
                            case a@ SellAll(_) => a.toString()
                            case a@ Sell(_)    => a.toString()
                            case a       => "Successfuly " + a
                        }
                        case StringResponse(str) => str

                        /* This is only returned by a buy event */
                        case TransactionResponse(sa, dollars, shares) =>
                            "Bought %s shares of %s for %s" format (shares.###(), sa.ticker, dollars.$)
                        case Failed(msg) => msg
                    }
                }

            case Failure(_, _) => Seq(huh)
            case Error(_, _)   => Seq(huh)
        }
    }
}

