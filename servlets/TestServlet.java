
package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import java.util.*;
import model.*;
import model.schema.*;
import scala.math.*;
import com.google.gson.*;

import net.liftweb.common.Loggable;

public class TestServlet extends HttpServlet {
    
    public static class Response {
        public double cash;
        public List<ResponseAsset> assets;
    }
    
    public static class ResponseAsset {
        public String ticker;
        public double shares;
        public double price;
        public double dollars;
    }
    
    /*
    @Override
    public void doPost(
        HttpServletRequest request,
        HttpServletResponse response
    )
        throws ServletException, IOException
    {
        PrintWriter out = response.getWriter();
        Response resp = new Response();
        
        User user = Schema.ensureUser("ellbur_k_a");
        Portfolio port = user.getMainPortfolio();
        
        resp.cash = port.cash().dollars().doubleValue();
        resp.assets = new ArrayList<ResponseAsset>();
        
        for (StockAsset asset : port.getMyStockAssets()) {
            BigDecimal shares  = asset.shares().shares();
            BigDecimal price   = asset.price().price();
            BigDecimal dollars = asset.dollars().dollars();
            
            ResponseAsset rAsset = new ResponseAsset();
            rAsset.ticker  = asset.ticker();
            rAsset.shares  = shares.doubleValue();
            rAsset.price   = price.doubleValue();
            rAsset.dollars = dollars.doubleValue();
            
            resp.assets.add(rAsset);
        }
        
        String json = new Gson().toJson(resp);
        out.println(json);
    }
    
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
    */
}

