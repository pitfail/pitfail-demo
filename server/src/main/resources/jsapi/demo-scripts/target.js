
output.html("")

var targets = [
    {ticker: "MSFT", dollars: 5000},
    {ticker: "AAPL", dollars: 8000},
    {ticker: "F",    dollars: 9000}
]

for (i in targets) (function(target) {
    var target = targets[i]
    var ticker = target.ticker
    var dollars = target.dollars
    
    howManyDollarsDoIOwn(ticker, function(current) {
        var error = current - dollars
        if (error/dollars < -.10) {
            output.append("Buying " + (-error) + " of " + ticker + "<br/>")
            buyDollars(ticker, -error)
        }
        else if (error/dollars > +.10) {
            output.append("Selling " + (error) + " of " + ticker + "<br/>")
            sellDollars(ticker, error)
        }
    })
})(targets[i])

