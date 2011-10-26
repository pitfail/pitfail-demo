
package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import java.util.*;
import model.*;
import model.Schema.*;

public class TestServlet extends HttpServlet {
    
    @Override
    public void init()
        throws ServletException
    {
    }
    
    @Override
    public void destroy() {
    }
    
    @Override
    public void doGet(
        HttpServletRequest request,
        HttpServletResponse response
    )
        throws ServletException, IOException
    {
    }
    
    @Override
    public void doPost(
        HttpServletRequest request,
        HttpServletResponse response
    )
        throws ServletException, IOException
    {
        PrintWriter out = response.getWriter();
        
        // Get the 10 most recent events
        List<NewsEvent> events = Schema.getRecentEvents(10);
        for (NewsEvent event : events) {
            out.printf("%s\n", event);
        }
        
        // Buy a stock
        User user = Schema.ensureUser("ellbur_k_a");
        Portfolio port = user.getMainPortfolio();
        port.buyStock("ABC", new Dollars("23.00"));
        
        try {
            port.sellStock("ABC", new Dollars("23.00"));
        }
        catch (NotEnoughShares e) {
            out.println("Failed cause you don't have enough cash");
        }
        
        out.println();
        out.printf("Cash: %s\n", port.cash().dollars());
        for (StockAsset asset : port.getMyStockAssets()) {
            out.printf("Asset: %s\n", asset);
            out.printf("Shares = %s\n", asset.shares().shares());
        }
    }
}

