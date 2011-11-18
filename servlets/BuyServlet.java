
package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import java.util.*;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.sql.DriverManager;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import model.*;

/**
 * <code>HttpServlet</code> responsible for buying stocks.
 * 
 * @author Roma, Sonu
 * 
 */
public class BuyServlet extends HttpServlet {
	private static final long serialVersionUID = -7287781887462285268L;
	
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
System.out.println("buy called  ************************************************************ AAAAAAAAAAAAAAAAAHHHHHHHHHHHHHH");

		String userId = request.getParameter("userid");
		String stockSymbol = request.getParameter("ticker");
		String volume = request.getParameter("volume");

		try {
                operations.userBuyStock(userId, stockSymbol, new Dollars(volume));
                        
				PrintWriter out = response.getWriter();
				out.printf("success");
				
				response.setContentType("text/html");
		    	ObjectOutputStream oos = new ObjectOutputStream(response.getOutputStream());
//			    String respond = "Successful Buy";
			    oos.writeObject("Successful Buy");
			    oos.flush();
			    oos.close();
		} catch (Exception e) {
			e.printStackTrace();
			
		}

	}
}
