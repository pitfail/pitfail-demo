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
import java.sql.Connection;
import java.sql.DriverManager;

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
public class SellServlet extends HttpServlet {

    /*
	private static final long serialVersionUID = -7287781887462285268L;
	
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		PrintWriter out = response.getWriter();			
		String userId = request.getParameter("userid");
		String stockSymbol = request.getParameter("ticker");
        


		try {
			
				User user = ensureUser(userId);
				Portfolio port = user.mainPortfolio().fetch(portfolios());
				port.sellAll(stockSymbol);

				out.printf("success");

				response.setContentType("text/html");
		    	ObjectOutputStream oos = new ObjectOutputStream(response.getOutputStream());
			    String respond = "Successful Sell";
			    oos.writeObject(respond);
			    oos.flush();
			    oos.close();
		} catch (Exception e) {
			e.printStackTrace();
			
		}

	}
    */
}
