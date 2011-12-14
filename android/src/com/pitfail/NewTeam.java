package com.pitfail;

import java.util.ArrayList;
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
import android.content.Intent;
import android.graphics.Color;
import android.graphics.LightingColorFilter;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;

public class NewTeam extends Activity implements OnClickListener {

	EditText portfolio, invite;
	Button inviteButton;
	Activity activity;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		activity = this;
		super.onCreate(savedInstanceState);
		setContentView(R.layout.teamportfolio);
		//Set background as white
		LinearLayout layout = (LinearLayout) findViewById(R.id.teamportfoliolayout);
		layout.setBackgroundColor(Color.WHITE);
		
		portfolio = (EditText) findViewById(R.id.teamportfolioName);
		invite = (EditText) findViewById(R.id.inviteUserText);
		inviteButton = (Button) findViewById(R.id.inviteButton);
		inviteButton.getBackground().setColorFilter(
				new LightingColorFilter(0xFF000000, 0xff00CC33));
		inviteButton.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		String url = "http://" + getString(R.string.server)
				+ ":8080/servlet/newportfolio";
		HttpPost get = new HttpPost(url);
		DefaultHttpClient client = new DefaultHttpClient();

		List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(4);
		nameValuePairs.add(new BasicNameValuePair("userid", getString(R.string.user)));
		nameValuePairs.add(new BasicNameValuePair("portfolio", portfolio
				.getText().toString()));
		nameValuePairs.add(new BasicNameValuePair("invite", invite.getText()
				.toString()));

		try {
			get.setEntity(new UrlEncodedFormEntity(nameValuePairs));

			// Execute HTTP Post Request
			HttpResponse response = client.execute(get);
			HttpEntity rp = response.getEntity();

			String originalContent = EntityUtils.toString(rp);
			System.out.println(originalContent);

		} catch (Exception e) {
			e.printStackTrace();

		}
		
		Intent intent = new Intent(activity, HomeScreen.class);
		startActivity(intent);
	}
}
