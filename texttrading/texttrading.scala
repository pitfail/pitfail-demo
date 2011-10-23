
package texttrading

trait Backend {
    def perform(request: Request): Response
}

trait Frontend {
    def messages: Iterator[String]
}

