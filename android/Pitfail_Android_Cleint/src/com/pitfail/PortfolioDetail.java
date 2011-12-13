package com.pitfail;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;

import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.widget.LinearLayout;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TextView;
import android.widget.TableRow.LayoutParams;

public class PortfolioDetail extends Activity {

	TextView cashText, portfolioHeader;
	String selectedPortfolio;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.portfoliodetail);
		LinearLayout layout = (LinearLayout) findViewById(R.id.portfoliodetaillayout);
		// ImageView image = (ImageView) findViewById(R.id.imageView1);
		layout.setBackgroundColor(Color.WHITE);
		selectedPortfolio = getIntent().getExtras().getString("userid");
		portfolioHeader = (TextView) findViewById(R.id.portfolioHeader);
		portfolioHeader.setText(selectedPortfolio);

		cashText = (TextView) findViewById(R.id.money1);

		// Hit the server to get values:
		String url = "http://" + getString(R.string.server)
				+ ":8080/servlet/fbgetportfolio";
		HttpPost get = new HttpPost(url);
		DefaultHttpClient client = new DefaultHttpClient();

		List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(4);
		nameValuePairs.add(new BasicNameValuePair("userid",selectedPortfolio));
		//nameValuePairs.add(new BasicNameValuePair("userid","sonu_pillai"));
		try {
			get.setEntity(new UrlEncodedFormEntity(nameValuePairs));

			// Execute HTTP Post Request
			HttpResponse response = client.execute(get);
			HttpEntity rp = response.getEntity();

			String originalContent = EntityUtils.toString(rp);
			StringTokenizer portfolioTokenizer = new StringTokenizer(originalContent, ",");
			
			StringTokenizer priceTokenizer;
			int i = 0;
			String cash = "";
			ArrayList<String> sharesName = new ArrayList<String>();
			ArrayList<String> sharesPrice = new ArrayList<String>();
			while (portfolioTokenizer.hasMoreTokens()) {
				String current = portfolioTokenizer.nextToken();
				if (i == 0) {
					cash = current.substring(current.indexOf(":"),
							current.length());
				} else {
					priceTokenizer = new StringTokenizer(current, ":");
					sharesName.add(priceTokenizer.nextToken());
					sharesPrice.add(priceTokenizer.nextToken());
				}
				i++;

			}
			// Set the values in table layout
			cashText.setText("$" + cash);
			// Display this data in the table for user profile
			TableLayout table = (TableLayout) findViewById(R.id.myTableLayout1);
			Iterator<String> priceIterator = sharesPrice.iterator();

			for (Iterator<String> sharesIterator = sharesName.iterator(); sharesIterator
					.hasNext();) {

				String shares = (String) sharesIterator.next();
				String price = (String) priceIterator.next();
				// Create new row to be added
				TableRow tr = new TableRow(this);
				tr.setLayoutParams(new LayoutParams(LayoutParams.FILL_PARENT,
						LayoutParams.WRAP_CONTENT));
				// Create the text view to be added
				TextView textView1 = new TextView(this);
				textView1.setText(shares);
				textView1.setTextColor(Color.BLACK);

				TextView textView2 = new TextView(this);
				textView2.setText(price);
				textView2.setTextColor(Color.BLACK);

				textView1.setLayoutParams(new LayoutParams(
						LayoutParams.FILL_PARENT, LayoutParams.WRAP_CONTENT));
				textView2.setLayoutParams(new LayoutParams(
						LayoutParams.FILL_PARENT, LayoutParams.WRAP_CONTENT));

				tr.addView(textView1);
				tr.addView(textView2);

				// Add text view to Table
				table.addView(tr, new TableLayout.LayoutParams(
						LayoutParams.FILL_PARENT, LayoutParams.WRAP_CONTENT));
			}
		} catch (Exception e) {

		}
	}

}
