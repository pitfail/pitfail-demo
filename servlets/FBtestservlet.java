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
private final static String _USERNAME = "username";
			    
protected void doPost( HttpServletRequest request,HttpServletResponse response )
	throws ServletException, IOException {
	
PrintWriter out = response.getWriter();
//String username = request.getParameter( _USERNAME );
//	String ticker= request.getParameter("ticker");
//	String volume = request.getParameter("volume");
//	System.out.println("***********************"+username+"***********************"+ticker+"*************************"+volume+"(((((((((((((((((((((((((((");
	
	 String userId = "";
	 String stockSymbol = "";
	 String volume = "";
	 System.out.println("))))))))))))))))))))))(((((((((((");
	 
	 
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

	  */ 
//System.out.println("***********"+userId+"***********");
//System.out.println("***********"+volume+"***********");


         /*
             operations.userBuyStock("sonu_pillai", "GOOG", new Dollars("2000"));
         */
	 } catch (Exception e) {  
			 e.printStackTrace();  
	 }  

	 System.out.println("********************"+userId+"**********************************");
	   
	response.setContentType("text/html");
	out.println("success");
	} 
}

