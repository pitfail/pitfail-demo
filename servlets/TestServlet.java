
package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import java.util.*;
import model.*;
import model.Schema.*;
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
    }
    
    @Override
    public void doPost(
        HttpServletRequest request,
        HttpServletResponse response
    )
        throws ServletException, IOException
    {
        PrintWriter out = response.getWriter();
        
        User user = Schema.ensureUser("ellbur_k_a");
        Portfolio port = user.getMainPortfolio();
        
        out.println();
        out.printf("Cash: %s\n", port.cash().dollars());
        for (StockAsset asset : port.getMyStockAssets()) {
            out.printf("Asset: %s\n", asset);
            
            BigDecimal shares  = asset.shares().shares();
            BigDecimal price   = asset.price().price();
            BigDecimal dollars = asset.dollars().dollars();
            
            out.printf("Shares  = %.0f\n", shares.doubleValue());
            out.printf("Price   = $%.2f\n", price.doubleValue());
            out.printf("Dollars = $%.2f\n", dollars.doubleValue());
        }
    }
}

