
package object texttrading {
    val commandIntro =
        """|"buy $150 of MSFT",
           |"sell 50 shares of MSFT",
           |"sell MSFT" (sells all),
           |"how much MSFT",
           |or "portfolio"
           |"""
        .stripMargin
}

