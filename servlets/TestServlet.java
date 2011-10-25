
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
        PrintWriter out = response.getWriter();
        
        // Get the 10 most recent events
        List<NewsEvent> events = Schema.getRecentEvents(10);
        for (NewsEvent event : events) {
            out.printf("%s\n", event);
        }
        
        // Buy a stock (in a GET no less!)
        User user = Schema.ensureUser("ellbur_k_a");
        Portfolio port = user.getMainPortfolio();
        port.buyStock("ABC", new Dollars("23.00"));
    }
    
    @Override
    public void doPost(
        HttpServletRequest request,
        HttpServletResponse response
    )
        throws ServletException, IOException
    {
    }
}

