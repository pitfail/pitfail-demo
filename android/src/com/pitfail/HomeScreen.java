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
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.LightingColorFilter;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.Spinner;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TableRow.LayoutParams;
import android.widget.TextView;
import android.widget.Toast;

public class HomeScreen extends Activity implements OnClickListener,
		TextWatcher {

	Activity activity;
	Spinner spinner;
	int indexOfUserNameForSpinner;
	// EditText ticker;
	Button search, sell, team, leaderboard;
	TextView cashText;
	AutoCompleteTextView ticker;
	String companies[] = { "MSFT", "GOOG", "AAPL", "BAC", "YHOO", "INFY",
			"July", "August", "September", "October", "November", "December" };

	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		activity = this;
		setContentView(R.layout.home);
		LinearLayout layout = (LinearLayout) findViewById(R.id.widget30);
		// ImageView image = (ImageView) findViewById(R.id.imageView1);
		layout.setBackgroundColor(Color.WHITE);

		/* Start Stock Polling Service */

		Intent serviceintent = new Intent(PollingService.class.getName());
		serviceintent.putExtra("userid", getString(R.string.user));
		startService(serviceintent);

		ticker = (AutoCompleteTextView) findViewById(R.id.tickersymbol);
		ticker.addTextChangedListener(this);
		ticker.setAdapter(new ArrayAdapter<String>(this,
				android.R.layout.simple_dropdown_item_1line, companies));

		search = (Button) findViewById(R.id.search);
		search.setOnClickListener(this);
		search.getBackground().setColorFilter(
				new LightingColorFilter(0xFF000000, 0xff00CC33));
		sell = (Button) findViewById(R.id.sell);
		sell.getBackground().setColorFilter(
				new LightingColorFilter(0xFF000000, 0xff00CC33));
		sell.setOnClickListener(new SellListener());

		team = (Button) findViewById(R.id.newTeam);
		team.getBackground().setColorFilter(
				new LightingColorFilter(0xFF000000, 0xff00CC33));
		team.setOnClickListener(new TeamListener());

		leaderboard = (Button) findViewById(R.id.leaderboard);
		leaderboard.getBackground().setColorFilter(
				new LightingColorFilter(0xFF000000, 0xff00CC33));
		leaderboard.setOnClickListener(new LeaderboardListener());

		cashText = (TextView) findViewById(R.id.money);

		// Get User Port Folio

		String url = "http://" + getString(R.string.server)
				+ ":8080/servlet/getportfolio";
		HttpPost get = new HttpPost(url);
		DefaultHttpClient client = new DefaultHttpClient();

		List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(4);
		nameValuePairs.add(new BasicNameValuePair("userid",
				getString(R.string.user)));

		String portfolioName = "";
		if (getIntent().getExtras() != null) {
			if (getIntent().getExtras().containsKey("portfolioname")) {
				portfolioName = (String) getIntent().getExtras().get(
						"portfolioname");
			}
		}
		if (portfolioName == null || portfolioName == "")
			portfolioName = getString(R.string.user);
		nameValuePairs.add(new BasicNameValuePair("portfolioname",
				portfolioName));

		try {
			get.setEntity(new UrlEncodedFormEntity(nameValuePairs));

			// Execute HTTP Post Request
			HttpResponse response = client.execute(get);
			HttpEntity rp = response.getEntity();

			String originalContent = EntityUtils.toString(rp);

			// Parse portfolio response
			StringTokenizer mainTokenizer = new StringTokenizer(
					originalContent, "-");

			String portfolioNames = mainTokenizer.nextToken();
			String portfolios = mainTokenizer.nextToken();

			//String leagues = mainTokenizer.nextToken();
			StringTokenizer portfolioTokenizer = new StringTokenizer(
					portfolios, ",");
			/*StringTokenizer leagueTokenizer = new StringTokenizer(leagues, ",");
			;*/
			StringTokenizer priceTokenizer;
			int i = 0;
			String cash = "";
			ArrayList<String> sharesName = new ArrayList<String>();
			ArrayList<String> sharesPrice = new ArrayList<String>();
			ArrayList<String> portfolioNameList = new ArrayList<String>();
			while (portfolioTokenizer.hasMoreTokens()) {
				String current = portfolioTokenizer.nextToken();
				if (i == 0) {
					cash = current.substring(current.indexOf(":"),
							current.length());
				} else {
					priceTokenizer = new StringTokenizer(current, ":");
					sharesName.add(priceTokenizer.nextToken());
					sharesPrice.add(priceTokenizer.nextToken());
					// shares.add(current);
				}
				i++;

			}
			/*while (leagueTokenizer.hasMoreTokens()) {
				String ranks = leagueTokenizer.nextToken();
				rankTokenizer = new StringTokenizer(ranks, ":");
				rankList.add(rankTokenizer.nextToken());
				portfolioList.add(rankTokenizer.nextToken());
			}*/

			// Add the portfolio names
			StringTokenizer portfolioNamesTokenizer = new StringTokenizer(
					portfolioNames, ":");
			while (portfolioNamesTokenizer.hasMoreTokens()) {
				String currentPortfolioName = portfolioNamesTokenizer
						.nextToken();
				portfolioNameList.add(currentPortfolioName);
			}

			Spinner spinner = (Spinner) findViewById(R.id.spinner);
			String[] portfolioNamesArray = new String[portfolioNameList.size()];
			int j = 0;
			for (Iterator iterator = portfolioNameList.iterator(); iterator
					.hasNext();) {
				String string = (String) iterator.next();
				portfolioNamesArray[j] = string;
				j++;
			}
			ArrayAdapter<String> spinnerAdapter = new ArrayAdapter<String>(this,
					android.R.layout.simple_spinner_item, portfolioNamesArray);
			spinner.setAdapter(spinnerAdapter);
			// Set listener on spinner
			spinner.setOnItemSelectedListener(new SpinnerListener());
			//set the default according to value
			indexOfUserNameForSpinner = portfolioNamesArray.length -1;
			spinner.setSelection(indexOfUserNameForSpinner);

			cashText.setText("$" + cash);
			// Display this data in the table for user profile
			TableLayout table = (TableLayout) findViewById(R.id.myTableLayout);
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

			// Display this data in the table for user profile
			

		} catch (Exception e) {
			e.printStackTrace();

		}

	}

	public void onClick(View v) {
		String url = "http://finance.yahoo.com/d/quotes.csv?s="
				+ ticker.getText() + "&f=nsl1hgp2op";

		HttpPost post = new HttpPost(url);
		DefaultHttpClient client = new DefaultHttpClient();

		try {

			// Execute HTTP Post Request
			HttpResponse response = client.execute(post);
			HttpEntity rp = response.getEntity();

			String originalContent = EntityUtils.toString(rp);

			StringTokenizer tokenizer = new StringTokenizer(originalContent,
					",");
			ArrayList<String> tickerValues = new ArrayList<String>();

			while (tokenizer.hasMoreElements()) {
				tickerValues.add(tokenizer.nextToken());
			}

			Intent intent = new Intent(activity, BuyDetail.class);
			intent.putExtra("ticker", ticker.getText());
			intent.putExtra("value", tickerValues);
			startActivity(intent);

		} catch (Exception e) {
			e.printStackTrace();

		}
	}

	/**
	 * Listener for handling a new team entry
	 * 
	 * @author Roma
	 * 
	 */
	class TeamListener implements OnClickListener {

		@Override
		public void onClick(View v) {
			Intent intent = new Intent(activity, NewTeam.class);
			startActivity(intent);

		}
	}

	class LeaderboardListener implements OnClickListener {

		@Override
		public void onClick(View v) {
			Intent intent = new Intent(activity, LeaderBoard.class);
			startActivity(intent);
		}
	}

	class SellListener implements OnClickListener {

		public void onClick(View v) {

			// Show the Dialog to get volume of shares from user
			Context mContext = activity;
			final Dialog dialog = new Dialog(mContext);
			dialog.setContentView(R.layout.selldialog);
			Button buyButton = (Button) dialog.findViewById(R.id.sellButton);
			final EditText volume = (EditText) dialog
					.findViewById(R.id.sellticker);
			dialog.show();

			dialog.setTitle("Sell Shares For");
			Button cancelSell = (Button) dialog
					.findViewById(R.id.cancelSellButton);
			cancelSell.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					dialog.dismiss();
				}
			});

			buyButton.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					final String tickerStr = volume.getText().toString()
							.toUpperCase();

					Toast.makeText(activity, "Selling Stock", Toast.LENGTH_LONG)
							.show();

					String url = "http://" + getString(R.string.server)
							+ ":8080/servlet/sellservlet";

					HttpPost get = new HttpPost(url);
					DefaultHttpClient client = new DefaultHttpClient();

					List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(
							4);
					nameValuePairs.add(new BasicNameValuePair("ticker",
							tickerStr));
					nameValuePairs.add(new BasicNameValuePair("userid",
							getString(R.string.user)));

					try {
						get.setEntity(new UrlEncodedFormEntity(nameValuePairs));

						// Execute HTTP Post Request
						HttpResponse response = client.execute(get);
						HttpEntity rp = response.getEntity();

						String originalContent = EntityUtils.toString(rp);
						if (originalContent.equalsIgnoreCase("success")) {
							Toast.makeText(activity,
									"Sold all stocks for " + tickerStr,
									Toast.LENGTH_LONG).show();
						} else {
							Toast.makeText(
									activity,
									"There was an error selling the stocks. Try again!"
											+ originalContent,
									Toast.LENGTH_LONG).show();
						}
						Intent intent = new Intent(activity, HomeScreen.class);
						startActivity(intent);

					} catch (Exception e) {
						e.printStackTrace();

					}
				}

			});

		}
	}

	@Override
	public void afterTextChanged(Editable s) {
	}

	@Override
	public void beforeTextChanged(CharSequence s, int start, int count,
			int after) {
	}

	@Override
	public void onTextChanged(CharSequence s, int start, int before, int count) {
	}

	/**
	 * Handler for spinner of portfolio names
	 * 
	 * @author Roma
	 * 
	 */
	class SpinnerListener implements OnItemSelectedListener {

		@Override
		public void onItemSelected(AdapterView<?> parent, View view,
				int position, long id) {
			String selectedPortfolioName = parent.getItemAtPosition(position)
					.toString();
		//	spinner.setSelection(indexOfUserNameForSpinner);
			
			if(!selectedPortfolioName.equals(getString(R.string.user))){
				Intent intent = new Intent(activity, PortfolioDetail.class);
				intent.putExtra("userid", selectedPortfolioName);
				activity.startActivity(intent);
			}
		}

		@Override
		public void onNothingSelected(AdapterView<?> arg0) {

		}

	}
}