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

/**
 * Activity to display Leader board
 * 
 * @author Roma
 * 
 */
public class LeaderBoard extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.leaderboard);
		LinearLayout layout = (LinearLayout) findViewById(R.id.leaderboardLayout);
		// ImageView image = (ImageView) findViewById(R.id.imageView1);
		layout.setBackgroundColor(Color.WHITE);

		String url = "http://" + getString(R.string.server)
				+ ":8080/servlet/leaderboard";
		HttpPost get = new HttpPost(url);
		DefaultHttpClient client = new DefaultHttpClient();

		List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(4);
		nameValuePairs.add(new BasicNameValuePair("userid",
				getString(R.string.user)));
		try {
			get.setEntity(new UrlEncodedFormEntity(nameValuePairs));

			// Execute HTTP Post Request
			HttpResponse response = client.execute(get);
			HttpEntity rp = response.getEntity();

			String originalContent = EntityUtils.toString(rp);
			
			StringTokenizer leagueTokenizer = new StringTokenizer(originalContent, ",");
			StringTokenizer rankTokenizer;
			ArrayList<String> portfolioList = new ArrayList<String>();
			ArrayList<String> rankList = new ArrayList<String>();
			
			while (leagueTokenizer.hasMoreTokens()) {
				String ranks = leagueTokenizer.nextToken();
				rankTokenizer = new StringTokenizer(ranks, ":");
				rankList.add(rankTokenizer.nextToken());
				portfolioList.add(rankTokenizer.nextToken());
			}

			TableLayout table2 = (TableLayout) findViewById(R.id.leaderboardTable);
			Iterator<String> portfolioIterator = portfolioList.iterator();

			for (Iterator<String> rankIterator = rankList.iterator(); rankIterator
					.hasNext();) {

				String rank = (String) rankIterator.next();
				String portfolio = (String) portfolioIterator.next();

				// Create new row to be added
				TableRow tr1 = new TableRow(this);
				tr1.setLayoutParams(new LayoutParams(LayoutParams.FILL_PARENT,
						LayoutParams.WRAP_CONTENT));
				// Create the text view to be added
				TextView rankView = new TextView(this);
				rankView.setText(rank);
				rankView.setTextColor(Color.BLACK);

				TextView portfolioView = new TextView(this);
				portfolioView.setText(portfolio);
				portfolioView.setTextColor(Color.BLACK);

				rankView.setLayoutParams(new LayoutParams(LayoutParams.FILL_PARENT,
						LayoutParams.WRAP_CONTENT));
				portfolioView.setLayoutParams(new LayoutParams(
						LayoutParams.FILL_PARENT, LayoutParams.WRAP_CONTENT));
				tr1.addView(rankView);
				tr1.addView(portfolioView);

				// Add text view to Table
				table2.addView(tr1, new TableLayout.LayoutParams(
						LayoutParams.FILL_PARENT, LayoutParams.WRAP_CONTENT));
			}
		} catch (Exception e) {

		}
	}
}
