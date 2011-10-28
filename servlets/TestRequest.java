
package servlets;

import java.net.*;
import java.io.*;
import com.google.gson.*;

public class TestRequest {
    
    public static void main(String[] args)
        throws Throwable
    {
        String host = "localhost";
        int port = 8080;
        String service = "/servlet/testservlet";
        
        URL url = new URL("http", host, port, service);
        HttpURLConnection con = (HttpURLConnection) url.openConnection();
        con.setRequestMethod("POST");
        
        Reader reader = new InputStreamReader(con.getInputStream());
        TestServlet.Response resp = new Gson().fromJson(reader, TestServlet.Response.class);
        
        System.out.printf("Cash: %.2f\n", resp.cash);
        for (TestServlet.ResponseAsset asset : resp.assets) {
            System.out.printf("%s ($%.2f/sh) $%.2f\n",
                asset.ticker,
                asset.price,
                asset.dollars
            );
        }
    }
    
}

