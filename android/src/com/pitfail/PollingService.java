package com.pitfail;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;

public class PollingService extends Service {
 
  private static final String TAG = PollingService.class.getSimpleName();
 
  private Timer timer;
  int UPDATE_ID = 0;
 
  private TimerTask updateTask = new TimerTask() {
    @Override
    public void run() {
      String updates = "";
      Log.i(TAG, "Timer task doing work");
      
      String url = "http://" + getString(R.string.server)
		+ ":8080/servlet/stockupdates";
      HttpPost get = new HttpPost(url);
      DefaultHttpClient client = new DefaultHttpClient();

      List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(4);
      nameValuePairs.add(new BasicNameValuePair("userid", getString(R.string.user)));

      try {
    	  	get.setEntity(new UrlEncodedFormEntity(nameValuePairs));

    	  	// Execute HTTP Post Request
    	  	HttpResponse response = client.execute(get);
    	  	HttpEntity rp = response.getEntity();

    	  	String originalContent = EntityUtils.toString(rp);

    	  	// Parse Stock Updates response
    	  	if(originalContent.equals("NULL")){
    	  		Log.i(TAG, "No Stock updates");
    	  	}
    	  	else{
    	  		StringTokenizer tokenizer = new StringTokenizer(originalContent,",");
       	  		ArrayList<String> shares = new ArrayList<String>();
    	  		while (tokenizer.hasMoreTokens()) {
    	  			String current = tokenizer.nextToken();
    	  			updates = updates.concat(current+"-----");
    	  		}
    	  		
    	  		/*Use Notification manager to display updates stocks */
    	  		
    	  		String ns = Context.NOTIFICATION_SERVICE;
    	  		NotificationManager mNotificationManager = (NotificationManager) getSystemService(ns);
    	  		
    	  		
    	  		// Instantiate the notification
    	  		
    	  		int icon = R.drawable.pitfail;
    	  		CharSequence tickerText = "Stock-Update";
    	  		long when = System.currentTimeMillis();

    	  		Notification notification = new Notification(icon, tickerText, when);
    	  		notification.ledARGB = 0xff00ff00;
    	  		notification.ledOnMS = 300;
    	  		notification.ledOffMS = 1000;
    	  		notification.flags |= Notification.FLAG_SHOW_LIGHTS;
    	  		notification.flags |= Notification.FLAG_AUTO_CANCEL;
    	  		
    	  		
    	  		//notification.defaults |= Notification.DEFAULT_LIGHTS;
    	  		
    	  		//Define the notification's message and PendingIntent:
    	  		
    	  		Context context = getApplicationContext();
    	  		CharSequence contentTitle = "Stock notification";
    	  		CharSequence contentText = updates;
    	  		Intent notificationIntent = new Intent(context,HomeScreen.class); //Intent.ACTION_VIEW);
    	  		PendingIntent contentIntent = PendingIntent.getActivity(context, 0, notificationIntent, 0);

    	  		notification.setLatestEventInfo(context, contentTitle, contentText, contentIntent);
    	  		
    	  		UPDATE_ID++;
    	 // 		notification.number = UPDATE_ID;
    	  		mNotificationManager.notify(1, notification);
    	  		
    	  	}
      }catch (Exception e) {
			e.printStackTrace();

		}
    }
  };
 
  @Override
  public IBinder onBind(Intent intent) {
    // TODO Auto-generated method stub
    return null;
  }
 
  @Override
  public void onCreate() {
    super.onCreate();
    Log.i(TAG, "Service creating");
 
    timer = new Timer("StockUpdateTimer");
    timer.schedule(updateTask, 1000L, 60 * 1000L);
  }
 
  @Override
  public void onDestroy() {
    super.onDestroy();
    Log.i(TAG, "Service destroying");
 
    timer.cancel();
    timer = null;
  }
}
