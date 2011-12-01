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
import scala.math.*;

import model.*;
import scala.math.BigDecimal;

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
		System.out.println("###############"+userId+"##################");
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
                        purchasePrice = asset.purchasePrice().price();
						myportfolio = myportfolio.concat(","+asset.ticker()+":"+dollars.doubleValue());
				}
				out.printf("%s",myportfolio);

				System.out.println("*********************************************"+myportfolio);



		} catch (Exception e) {
			e.printStackTrace();
			
		}

	}
}
