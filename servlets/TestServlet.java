package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import model.schema;
import static model.schema.*;
import model.*;
<<<<<<< HEAD
import scala.collection.*;
import static scala.collection.JavaConversions.*;
import java.util.*;
import scala.math.BigDecimal;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.sql.DriverManager;
=======
import scala.math.*;
import com.google.gson.*;
>>>>>>> 0ac5ea52bca94da03a4411cfed7c55036a379bf5

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * <code>HttpServlet</code> responsible for buying stocks.
 * 
 * @author Roma, Sonu
 * 
 */
public class TestServlet extends HttpServlet {
    
<<<<<<< HEAD

	private static final long serialVersionUID = -7287781887462285268L;
	
	int i = 0;
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		

/*		String userId = request.getParameter("userid");
		String stockSymbol = request.getParameter("ticker");
		String volume = request.getParameter("volume");

*/		try {
			
/*				User user = ensureUser(userId);
				Portfolio port = user.mainPortfolio().fetch(portfolios());
				port.buyStock(stockSymbol, new Dollars(volume));

*/
				PrintWriter out = response.getWriter();
				out.printf("success");
				
				response.setContentType("text/html");
		    	ObjectOutputStream oos = new ObjectOutputStream(response.getOutputStream());
//			    String respond = "Successful Buy";
			    oos.writeObject(i);
			    oos.flush();
			    oos.close();
				i++;
		} catch (Exception e) {
			e.printStackTrace();
			
		}

	}
    
=======
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
    
    @Override
    public void doPost(
        HttpServletRequest request,
        HttpServletResponse response
    )
        throws ServletException, IOException
    {
        try {
            System.err.println("Running TestServlet");
            
            PrintWriter out = response.getWriter();
            Response resp = new Response();
            
            UserSchema.Portfolio port = operations.getUserPortfolio("ellbur_k_a");
            
            resp.cash = port.cash().dollars().doubleValue();
            resp.assets = new ArrayList<ResponseAsset>();
            
            for (StockSchema.StockAsset asset : port.getMyStockAssets()) {
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
            
            System.err.println("Done TestServlet");
        }
        catch (RuntimeException e) {
            e.printStackTrace();
            throw e;
        }
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
>>>>>>> 0ac5ea52bca94da03a4411cfed7c55036a379bf5
}
