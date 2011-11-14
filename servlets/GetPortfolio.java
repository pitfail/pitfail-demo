package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import model.schema;
import static model.schema.*;
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
    /*
    
	private static final long serialVersionUID = -7287781887462285268L;
	

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		PrintWriter out = response.getWriter();			
		
		String userId = request.getParameter("userid");
		System.out.println("###############"+userId+"##################");
		BigDecimal shares;
		BigDecimal price;
		BigDecimal dollars;
		String myportfolio = "";
       
		try {
			
				User user = ensureUser(userId);
				Portfolio port = user.mainPortfolio().fetch(portfolios());
				BigDecimal cash = port.cash().dollars();
				myportfolio = myportfolio.concat("Cash:"+cash);
				
				for (StockAsset asset : port.getMyStockAssets()){
						shares = asset.shares().shares();
						dollars = asset.dollars().dollars();
						myportfolio = myportfolio.concat(","+asset.ticker()+":"+dollars.doubleValue());
				}
				out.printf("%s",myportfolio);

				System.out.println("*********************************************"+myportfolio);



		} catch (Exception e) {
			e.printStackTrace();
			
		}

	}
*/
}
