package New;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.net.URLConnection;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Date;
import java.util.List;
import java.util.StringTokenizer;


import com.restfb.BinaryAttachment;
import com.restfb.DefaultFacebookClient;
import com.restfb.FacebookClient;
import com.restfb.Parameter;
import com.restfb.types.*;
import com.restfb.*;
import com.google.*;

//stocks = stockretriever.StockRetriever()

// Replace the values with your unique API Key/Secret and Session Key!  
public class myFirstPost1{

	public final static String FACEBOOK_API_KEY = "109750979042759";  
	public final static String FACEBOOK_API_SECRET = "3e48a675b34484c285c3ffe91f99ebc5";  
	public final static String SESSION_KEY = "5aa04ad35c28a9887a85d1ef.1-100000542855489";  

	// Replace the User Id with the Admin User Id of the Facebook Page  
	public final static String USER_ID = "100000542855489";  


	public final static String FACEBOOK_PAGE_ID = "100000542855489";  // 
	//public static String MY_ACCESS_TOKEN = "AAABj0WQenccBABZBDeQOEViJyLrCpjcZC2ZAZAhWr0GxH9ipQ58tZCyKIm4i5v8oKT7YEXXQ4HfTPnLxZCQSetQ6lMriP7ZBjTsZBWUDSR5hZAAZDZD";
	public static String MY_ACCESS_TOKEN ="AAABlmd3ZC8k0BAHKSJqCAjLnZCv4gfWZBrgM9QVHaQy1ITUTDWqoFdTLNw2IrrIgv3xAzaeCAppQLXEeZAyjU4OsHEvgyjqZAGkQD2PCNdgZDZD";
	public static String accestoken ="AAABlmd3ZC8k0BADrs8BNcBdaLraBDvV6D58xTRfQtTjorduojUFlUAELHylYkcvfNZC3miIQkZC4fzzQZBr2xggRxkBhTqCSY1GLBiSZAJwI4PAslV2UR";
	public static void main(String args[]) throws Exception{  

		FacebookClient facebookClient = new DefaultFacebookClient(accestoken);

		FacebookClient publicOnlyFacebookClient = new DefaultFacebookClient();

		Page page = facebookClient.fetchObject("pitfail", Page.class);
		//accestoken = Page.getAccessToken();

		System.out.println("Page:" + page);
		//FetchObjectsResults fetchObjectsResults =
		// facebookClient.fetchObjects(Arrays.asList("me", "cocacola"), FetchObjectsResults.class);

		while(true)
		{

		Connection<Post> myFeed = facebookClient.fetchConnection("me/feed", Post.class);


		Thread.currentThread().sleep(1000);
		int counter=0;
		while (myFeed.getData().get(counter).getLikes()==null)
		{
			String msg= myFeed.getData().get(counter).getMessage();
			String idd = myFeed.getData().get(counter).getId();
			/////       Receives wall post and ID of that wall post.
			/// next loop will give me 4 parameters -- username, amount of shares, companyname, buy/sell

			StringTokenizer token = new StringTokenizer(msg, ":");
			int i = 0;

			//char quote = new google.finance.Quote();
			//quote.getQuotes(["GOOG","INTC"]);

			String username, operation,BuySell, message, op,company = null,dollars_text = null,volume;

			if (token.countTokens()==4 || token.countTokens()==3)
			{

				username = token.nextToken();
				operation= token.nextToken();



				if (operation.equalsIgnoreCase("buy") || operation.equalsIgnoreCase("sell"))
				{

					if (operation.equalsIgnoreCase("buy")){

						dollars_text = token.nextToken();
						volume=dollars_text;
						int shares = Integer.parseInt(dollars_text);

						company = token.nextToken();
						BuySell = "http://wonton.rutgers.edu:8080/servlet/buyservlet";
						op = "bought";
						message = "userid="+username+"&ticker="+company+"&volume="+volume+"r\n";
					}
					else {

						company= token.nextToken();
						// 	if(operation.equalsIgnoreCase("sell")){
						BuySell = "http://wonton.rugters.edu:8080/servlet/sellservlet";
						message = "userid="+username+"&ticker="+company+"\n";
						op= "sold";
					}


					URL url = new URL(BuySell);
					URLConnection conn = url.openConnection();
					conn.setDoOutput(true);

					BufferedWriter out = new BufferedWriter(new OutputStreamWriter(conn
							.getOutputStream()));
					out.write("userid="+username+"&ticker="+company+"\n");
					out.flush();
					out.close();
					BufferedReader in = new BufferedReader(new InputStreamReader(conn
							.getInputStream()));

					String response;
					String res="";
					while ((response = in.readLine()) != null) {
						res.concat(response);
					}
					in.close();



					if (operation.equalsIgnoreCase("buy"))
					{
						FacebookType publishMessageResponse =
								facebookClient.publish(idd+"/comments", FacebookType.class, Parameter.with(
										"message", "You baught "+company+" shares worth $"+dollars_text+"!"));
					}
					else
					{
						FacebookType publishMessageResponse =
								facebookClient.publish(idd+"/comments", FacebookType.class, Parameter.with(
										"message", "You sold all shares for  "+company));
					}
				}

				else
				{
					FacebookType publishMessageResponse =
							facebookClient.publish(idd+"/comments", FacebookType.class, Parameter.with(
									"message", "Please select the operation -- buy/sell/view portfolio (Please read page description for more information!)"));

				}
			}	
			else
			{
				if (token.countTokens()==2)
				{
					username = token.nextToken();
					operation= token.nextToken();

					if (operation.equalsIgnoreCase("portfolio"))
					{
						BuySell = "http://wonton.rugters.edu:8080/servlet/fbgetportfolio";
						URL url = new URL(BuySell);
						URLConnection conn = url.openConnection();
						conn.setDoOutput(true);

						BufferedWriter out = new BufferedWriter(new OutputStreamWriter(conn
								.getOutputStream()));
						out.write("userid="+username+"&ticker="+company+"\n");
						out.flush();
						out.close();
						BufferedReader in = new BufferedReader(new InputStreamReader(conn
								.getInputStream()));

						String response;
						String fullOne= "";
						response = in.readLine();

						/*    	        			while ((response = in.readLine()) != null) {
    	        			fullOne.concat(response);	
    	        			}
						 */  	        			in.close();

						 String newOne = response.replace(',', '\n');

						 
						 FacebookType publishMessageResponse =
								 facebookClient.publish(idd+"/comments", FacebookType.class, Parameter.with(
										 "message", "Your Portfolio: \n\n"+newOne));
					}


					if (operation.equalsIgnoreCase("leaderboard"))
					{
						BuySell = "http://wonton.rugters.edu:8080/servlet/leaderboard";
						URL url = new URL(BuySell);
						URLConnection conn = url.openConnection();
						conn.setDoOutput(true);

						BufferedWriter out = new BufferedWriter(new OutputStreamWriter(conn
								.getOutputStream()));
						out.write("userid="+username+"\n");
						out.flush();
						out.close();
						BufferedReader in = new BufferedReader(new InputStreamReader(conn
								.getInputStream()));

						String response;
						String fullOne= "";
						response = in.readLine();

						 	        			in.close();

						 String newOne = response.replace(',', '\n');

						
						 FacebookType publishMessageResponse =
								 facebookClient.publish(idd+"/comments", FacebookType.class, Parameter.with(
										 "message", "Leaderboard: \n\n"+newOne));
						 
						
					}
				}
				else
				{

					FacebookType publishMessageResponse =
							facebookClient.publish(idd+"/comments", FacebookType.class, Parameter.with(
									"message", "Please select the operation buy/sell/view portfolio (Please read page description for more information!"));
					//Please \nselect the operation\n  -- \n buy/sell/view portfolio (Please read page description for more information!)
				}



			}

			//System.out.println("Username:" + Useranme + "Operation:" + operation + "");


			//Date oneWeekAgo = new Date(System.currentTimeMillis() / 1000L - 60L * 60L * 24L * 7L);

			System.out.println("Filtered feed count: " + myFeed.getData().size());
			// Connections support paging and are iterable

			//FacebookType Boolean = facebookClient.publish(idd+"/likes", FacebookType.class);
			Boolean strResponse=facebookClient.publish(idd+"/likes",Boolean.class); 
			counter=counter+1;
		} 

		

		// System.out.println("Published message ID: " + publishMessageResponse.getId());

	}
}
}

// Holds results from a "fetchObjects" call.
// You need to write this class yourself!

/*
			public static class FetchObjectsResults {
			  @Facebook
			  User me;

			  // If the Facebook property name doesn't match
			  // the Java field name, specify the Facebook field name in the annotation.

			  @Facebook("cocacola")
			  Page page;
			}
 */




