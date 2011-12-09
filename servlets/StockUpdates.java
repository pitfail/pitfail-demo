package servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import java.util.*;

import java.io.IOException;
import java.io.ObjectOutputStream;


import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;


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
public class StockUpdates extends HttpServlet {
	private static final long serialVersionUID = -7287781887462285268L;
	

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		PrintWriter out = response.getWriter();			
		
		String userId = request.getParameter("userid");
		BigDecimal shares;
		BigDecimal price;
		BigDecimal dollars;
		String updatedstocks = "";
		String ticker = "";
//		String price = "60";
		int update = 0;
       
		try {
			    UserSchema.Portfolio port = operations.getUserPortfolio(userId);
			
				
				/*For testing read price from a file*/
				FileInputStream fstream = new FileInputStream("/home/sonu/PitFail/pitfail-demo/servlets/textfile.txt");
				DataInputStream in = new DataInputStream(fstream);
				BufferedReader br = new BufferedReader(new InputStreamReader(in));
				String strLine;
                                        
				for (StockSchema.StockAsset asset : port.getMyStockAssets()){
						ticker = asset.ticker();
						price = asset.averagePurchasePrice().price();
						

						/*Check from Yahoo if stock price has changed*/
						String url = "http://finance.yahoo.com/d/quotes.csv?s="+ ticker + "&f=l1";
						HttpPost post = new HttpPost(url);
						DefaultHttpClient client = new DefaultHttpClient();
						try {
																	
						// Execute HTTP Post Request
							HttpResponse stockresponse = client.execute(post);
							HttpEntity rp = stockresponse.getEntity();
							String originalContent = EntityUtils.toString(rp);
							if(originalContent.equals(price)){
									System.out.println("*********No Change********  %");
							}
							else{
									/*Read from file to test*/
									if ((strLine = br.readLine()) != null){
										originalContent = strLine;
									}

							
								updatedstocks = updatedstocks.concat(asset.ticker()+":"+originalContent+",");
								update = 1;
								// Update portfolio database with last notification entry
								System.out.println("************New Stock Price***********");
							}
						}catch (Exception e) {
							e.printStackTrace();
						}
				}
				if(update == 0)
						updatedstocks = "NULL";

				out.printf("%s",updatedstocks);
				
				in.close();
		} catch (Exception e) {
			e.printStackTrace();
			
		}

	}
}
