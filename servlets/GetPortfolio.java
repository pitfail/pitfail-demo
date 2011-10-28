package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import model.Schema;
import static model.Schema.*;
import model.*;
import scala.collection.*;
import static scala.collection.JavaConversions.*;
import java.util.*;
import scala.math.BigDecimal;

import java.io.IOException;
import java.io.ObjectOutputStream;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import scala.math.*;


/**
 * <code>HttpServlet</code> responsible for buying stocks.
 * 
 * @author Roma, Sonu
 * 
 */
public class GetPortfolio extends HttpServlet {

	private static final long serialVersionUID = -7287781887462285268L;
	

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		PrintWriter out = response.getWriter();			
		
		String userId = request.getParameter("userid");
		BigDecimal shares;
		BigDecimal price = asset.price().price();
		BigDecimal dollars = asset.dollars().dollars();
		String myportfolio = "";
       
		try {
			
				User user = ensureUser(userId);
				Portfolio port = user.mainPortfolio().fetch(portfolios());
				BigDecimal cash = port.cash().dollars();
				myportfolio = myportfolio.concat("Cash:"+cash);
				
				for (StockAsset asset : port.getMyStockAssets()){
						shares = asset.shares().shares();
						dollars = asset.dollars().dollars();
						myportfolio = myportfolio.concat(","+asset.ticker()+":"+shares.doubleValue());
				}
				out.printf("%s",myportfolio);

//				Iterator Count = Count.iterator();
//			while(i != size){
//		        	String value=(String)Count.next();
//					out.printf("Asset : %s\n",assetList.get(i));
//			    }




/*				response.setContentType("text/html");
		    	ObjectOutputStream oos = new ObjectOutputStream(response.getOutputStream());
			    String respond = "Portfolio extracted";
			    oos.writeObject(respond);
			    oos.flush();
			    oos.close();
*/		} catch (Exception e) {
			e.printStackTrace();
			
		}

	}
}
