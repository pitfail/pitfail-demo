package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
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

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.*;
import java.util.StringTokenizer;
import javax.servlet.*;
import javax.servlet.http.*;
public class FBtestservlet extends HttpServlet {
			    
protected void doPost( HttpServletRequest request,HttpServletResponse response )
	throws ServletException, IOException {
	
PrintWriter out = response.getWriter();
//	String ticker= request.getParameter("ticker");
//	String volume = request.getParameter("volume");
//	System.out.println("***********************"+username+"***********************"+ticker+"*************************"+volume+"(((((((((((((((((((((((((((");
	
	 String userId = request.getParameter("username");
	 String stockSymbol = request.getParameter("ticker");
	 
	 
	 //String results = "";  
	 try {  
/*			         
*/
/*			StringTokenizer token = new StringTokenizer(username,":");
			int i = 0;
			System.out.println("***********"+username+"***********");
			if (token.countTokens() == 4) {
				userId = token.nextToken();
				String operation = token.nextToken();
				volume = token.nextToken();
	//		int shares = Integer.parseInt(dollars_text);
				stockSymbol = token.nextToken();

            }

	/*		 userId = results.get(0);
			 stockSymbol = results.get(1);
			 volume = results.get(2);

	  */       //User user = ensureUser("sonu_pillai");
//System.out.println("***********"+userId+"***********");
//System.out.println("***********"+volume+"***********");


         /*
			 Portfolio port = user.mainPortfolio().fetch(portfolios());
<<<<<<< HEAD
//			 port.buyStock("GOOG", new Dollars("2000")); 
=======
			 port.buyStock("GOOG", new Dollars("2000")); 
         */
>>>>>>> 1d7d8d61f61fc4cb52138433fe219de6fe004e04
	 } catch (Exception e) {  
			 e.printStackTrace();  
	 }  

	 System.out.println("********************"+userId+"****************"+stockSymbol+"******************");
	   
	response.setContentType("text/html");
	out.println("success");
	} 
}

