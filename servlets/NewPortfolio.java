
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
public class NewPortfolio extends HttpServlet {
	private static final long serialVersionUID = -7287781887462285268L;
	
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		

		PrintWriter out = response.getWriter();
		String newPortfolio = request.getParameter("portfolio");
		String invite = request.getParameter("invite");
		String userId = request.getParameter("userid");
		// Get a user
		UserSchema.User user = operations.getUser(userId);


		try {
				 UserSchema.Portfolio newPort = user.userCreatePortfolio(newPortfolio);
		        // Invite someone to it
			    try {
					     newPort.userInviteUser(invite);
		            }
			    catch (SchemaErrors$NoSuchUser$ e) {
					    out.printf("No user named %s\n",invite);
		     }

				out.printf("success");
				
		} catch (Exception e) {
			e.printStackTrace();
			out.printf("failed");
			
		}

	}
}
