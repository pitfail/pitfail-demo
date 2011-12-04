
var trader = "pitfail"

news(function(news) {
    var table = $('<table>'); output.append(table)
    
    for (i in news) (function(event) {
        if (event.trader != trader) return;
        if (!(event.action=="bought" || event.action=="sold")) return;
        
        var row = $('<tr>'); table.append(row)
    
        var td1 = $('<td>'); row.append(td1)
        var td2 = $('<td>'); row.append(td2)
    
        var description = trader + " " + event.action + " "
            + event.shares + " of " + event.ticker
        td1.text(description)
        
        var a = $('<a href="#"/>'); td2.append(a)
        a.text('Imitate')
        a.click(function() {
            if (event.action == "bought") buyShares(event.ticker, event.shares)
            else if (event.action == "sold") sellShares(event.ticker, event.shares)
        })
    })(news[i])
})

