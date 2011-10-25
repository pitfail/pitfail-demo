
package object texttrading {
    
    val commandIntro =
        """|   buy $150 of MSFT
           |   sell $50 of MSFT
           |   sell MSFT               -- Sell all your MSFT stock
           |   buy 10 shares of BMY
           |   portfolio               -- See your current portfolio
           |   help                    -- Get more info
           |"""
        .stripMargin
        
}

