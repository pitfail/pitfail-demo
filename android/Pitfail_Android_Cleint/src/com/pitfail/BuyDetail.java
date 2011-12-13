package com.pitfail;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

public class BuyDetail extends Activity{

	TextView tickerName;
	Button buy;
	Activity activity;
	String tickerString;
	ArrayList<String> valueString;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		activity = this;
		setContentView(R.layout.tickerdetail);
		LinearLayout layout = (LinearLayout) findViewById(R.id.widgetmain);
		layout.setBackgroundColor(Color.WHITE);
		tickerName = (TextView) findViewById(R.id.ticketname);
		buy = (Button) findViewById(R.id.buy);
		buy.getBackground().setColorFilter(new LightingColorFilter(0xFF000000, 0xff00CC33));
		buy.setOnClickListener(new BuyListener());
		tickerString = (getIntent().getExtras().get("ticker")).toString();
		tickerName.setText(tickerString.toString().toUpperCase());
		valueString = (ArrayList<String>) getIntent().getExtras().get("value");
		
		/*--------------------- TO DISPLAY CHART------------------*/
		
	//	Context context = view.getContext();
		String ed = "http://ichart.finance.yahoo.com/t?s="
				+ tickerString;
		Drawable image = ImageOperations(ed.toString(),"image.png");
		ImageView imgView;
		imgView = (ImageView)findViewById(R.id.stockchart);
		imgView.setImageDrawable(image);
		
		/*--------------------- TO DISPLAY CHART------------------*/
		
		//Get all the Text Views for the table rows
		ArrayList<TextView> textViews = new ArrayList<TextView>();
		textViews.add((TextView) findViewById(R.id.name));
		textViews.add((TextView) findViewById(R.id.symbol));
		textViews.add((TextView) findViewById(R.id.lastTrade));
		textViews.add((TextView) findViewById(R.id.daysHigh));
		textViews.add((TextView) findViewById(R.id.daysLow));
		textViews.add((TextView) findViewById(R.id.changeInPercent));
		textViews.add((TextView) findViewById(R.id.open));
		textViews.add((TextView) findViewById(R.id.previousClose));

		int i =0;
		//Set all these values in the table
		for (Iterator iterator = valueString.iterator(); iterator.hasNext();) {
			TextView currentTextView = textViews.get(i);
			currentTextView.setText((String) iterator.next());
			i++;
		}
		
	}
	
	/*--------------------- TO DISPLAY CHART------------------*/
	private Drawable ImageOperations(String url, String saveFilename) {
		try {
			InputStream is = (InputStream) this.fetch(url);
			Drawable d = Drawable.createFromStream(is, "src");
			return d;
		} catch (MalformedURLException e) {
			e.printStackTrace();
			return null;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	
	public Object fetch(String address) throws MalformedURLException,IOException {
		URL url = new URL(address);
		Object content = url.getContent();
		return content;
	}
	
	
	
	/*--------------------- TO DISPLAY CHART------------------*/
	

	/**
	 * Handles buying of shares
	 * 
	 */
	class BuyListener implements OnClickListener {

		public void onClick(View v) {
			Toast.makeText(activity, "Buying Stock", Toast.LENGTH_LONG).show();
			final String url = "http://" + getString(R.string.server)
					+ ":8080/servlet/buyservlet";

			// Show the Dialog to get volume of shares from user
			Context mContext = activity;
			final Dialog dialog = new Dialog(mContext);
			dialog.setContentView(R.layout.buydialog);
			dialog.show();
			Button buyButton = (Button) dialog.findViewById(R.id.buyButton);
			buyButton.getBackground().setColorFilter(new LightingColorFilter(0xFF000000, 0xff00CC33));
			final EditText volume = (EditText) dialog.findViewById(R.id.buyvolume);
			Button cancelBuy = (Button) dialog.findViewById(R.id.cancelBuyButton);
			cancelBuy.getBackground().setColorFilter(new LightingColorFilter(0xFF000000, 0xff00CC33));

			dialog.setTitle("Buy Shares: " + tickerString.toUpperCase());
			cancelBuy.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					dialog.dismiss();
				}
			});
			
			buyButton.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					final String volumeStr = volume.getText().toString();
					HttpPost get = new HttpPost(url);
					DefaultHttpClient client = new DefaultHttpClient();
					tickerString = tickerString.toUpperCase();

					List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(
							4);
					nameValuePairs.add(new BasicNameValuePair("ticker",
							tickerString));
			/*		nameValuePairs.add(new BasicNameValuePair("value",
							valueString));*/
					nameValuePairs.add(new BasicNameValuePair("volume",
							volumeStr));
					nameValuePairs.add(new BasicNameValuePair("userid",
							getString(R.string.user)));

					try {
						get.setEntity(new UrlEncodedFormEntity(nameValuePairs));

						// Execute HTTP Post Request
						HttpResponse response = client.execute(get);
						HttpEntity rp = response.getEntity();

						String originalContent = EntityUtils.toString(rp);
						if (originalContent.equalsIgnoreCase("success")) {
							Toast.makeText(
									activity,
									"Bought stock for " + tickerString,
									Toast.LENGTH_LONG).show();
						} else {
							Toast
									.makeText(
											activity,
											"There was an error buying the stocks. Try again!",
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

	

}
