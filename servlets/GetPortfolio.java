package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import java.util.*;

import java.io.IOException;
import java.io.ObjectOutputStream;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;

import model.schema;
import static model.schema.*;
import model.*;
import scala.collection.*;
import static scala.collection.JavaConversions.*;
import java.util.*;
import scala.math.BigDecimal;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.sql.DriverManager;
import scala.math.*;
import com.google.gson.*;


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
		BigDecimal price;
		BigDecimal dollars;
        BigDecimal purchasePrice;
		String myportfolio = "";
       
		try {
			    UserSchema.Portfolio port = operations.getUserPortfolio(userId);
				BigDecimal cash = port.cash().dollars();
				myportfolio = myportfolio.concat("Cash:"+cash);
				                                         
				for (StockSchema.StockAsset asset : port.getMyStockAssets()){
						shares = asset.shares().shares();
						dollars = asset.dollars().dollars();
                        purchasePrice = asset.averagePurchasePrice().price();
						myportfolio = myportfolio.concat(","+asset.ticker()+":"+dollars.doubleValue());
				}
				UserSchema.User user = operations.getUser(userId);
				//Get the current portfolio
				UserSchema.Portfolio current = user.getCurrentPortfolio();
				//Get the league the portfolio belongs to
				UserSchema.League league = current.getLeague();
				// Get the 5 highest portfolios for the league
				myportfolio = myportfolio.concat("-");
				List<UserSchema.Portfolio> leaders = league.getLeaders(5);
				for(UserSchema.Portfolio p : leaders){
					myportfolio = myportfolio.concat(p.rank()+":"+p.name()+",");
					System.out.printf("#%d %s\n", p.rank(), p.name());
				}
				out.printf("%s",myportfolio);




		} catch (Exception e) {
			e.printStackTrace();
			
		}

	}
}
