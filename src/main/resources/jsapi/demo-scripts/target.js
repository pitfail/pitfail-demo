
var targets = [
    {'ticker': "MSFT", 'dollars': 5000},
    {'ticker': "AAPL", 'dollars', 8000},
    {'ticker': "F",    'dollars', 9000}
]

for (i in targets) {
    var target = targets[i]
    var ticker = target.ticker
    var dollars = target.dollars
    
    howManyDollars(ticker, function(current) {
        var error = current - target
        if (error/dollars < -.10) {
            buyDollars(ticker, error)
        }
        else if (error/dollars > +.10) {
            sellDollars(ticker, -error)
        }
    })
}

