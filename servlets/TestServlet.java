
package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import model.Schema;
import static model.Schema.*;

import scala.collection.*;
import static scala.collection.JavaConversions.*;
import java.util.*;
import scala.math.*;

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
        List<NewsEvent> events = seqAsJavaList(recentEvents(10));
        for (NewsEvent event : events) {
            out.printf("%s\n", event);
        }
        
        // Buy a stock (in a GET no less!)
        User user = ensureUser("ellbur_k_a");
        Portfolio port = user.mainPortfolio().fetch(portfolios());
        port.buyStock("ABC", BigDecimal.valueOf(23.00));
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

