//Brian Goodacre
/*In this class, an Actor requests that the newsletter's body is created and sent to a particular user.*/

package newsletter;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.io.*;
import java.net.MalformedURLException;

import com.itextpdf.text.*;
import com.itextpdf.text.pdf.*;
import com.lowagie.text.DocumentException;
import org.xhtmlrenderer.pdf.ITextRenderer;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

import model.schema;
import static model.schema.*;
import model.*;
import scala.collection.*;
import static scala.collection.JavaConversions.*;
import java.util.*;
import scala.math.BigDecimal;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.sql.DriverManager;
import scala.math.*;
import com.google.gson.*;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


public class create_newsletter {
	private static String newsletter_file_name = "newsletter.html";
	
	
	//write the body of the html file for returning
	public static File create_newsletter(String userID) throws IOException, DocumentException{
		String file_template = "newsletter_fancy.html";
		String[] html_text_template = read_in_html_template(file_template);//get template
		String[] html_text_edit=html_edit(html_text_template, userID); //edit template
		File f = text_to_html(html_text_edit,newsletter_file_name);//convert to html
		return f;
	}
	
	//create html file
	private static File text_to_html(	String[] body_text,
										String new_file_name) throws IOException{			
			FileWriter outFile = new FileWriter(new_file_name);
			PrintWriter out = new PrintWriter(outFile);
			
			int i=0;
			while(i<111 && body_text[i]!=null){
				out.println(body_text[i]);
				i++;
			}
			out.close();
			
			File f = new File(new_file_name);
			return f;
		}
	
	//customize html file
	private static String[] html_edit(String[] html_text_template, String userID){
		int i=0;
		String html_text[] = html_text_template;
		html_text[66]= get_name(userID);
		html_text[69]= get_date();
		html_text[80]= get_stocks(userID);
		html_text[87]= get_money(userID);
		return html_text;
	}
	
	/*Get field for newsletter*/
	private static String get_name(String userID){
		return userID;		
	}
	/*Get field for newsletter*/
	private static String get_date(){
		return getCurrentTime();
	}
	/*Get field for newsletter*/
	private static String get_stocks(String userID){
		UserSchema.User user = operations.getUser(userID);
		UserSchema.Portfolio current = user.getCurrentPortfolio();
		
		//for that returns the stocks I want
		String return_string="";
		
		for (StockSchema.StockAsset asset : current.getMyStockAssets()) {
			BigDecimal shares  = asset.shares().shares();
			BigDecimal price   = asset.price().price();
			BigDecimal dollars = asset.dollars().dollars();
			String ticker  = asset.ticker();
						
			return_string=return_string+"<tr><td>"+ticker+"</td><td>"+Double.toString(price.doubleValue())+"</td><td>"+Double.toString(dollars.doubleValue())+"</td><td>"+Double.toString(shares.doubleValue())+"</td></tr>";
            	}
		return return_string;
	}
	/*Get field for newsletter*/
	private static String get_money(String userID){
		UserSchema.User user = operations.getUser(userID);
		UserSchema.Portfolio current = user.getCurrentPortfolio();
		double cash = current.cash().dollars().doubleValue();
		return Double.toString(cash);
	}
	
	//generate email content
	private static String[] read_in_html_template(String file_name) throws IOException{
		FileReader fr = new FileReader(file_name);
		BufferedReader textReader = new BufferedReader(fr);
		int numberOfLines = 115;
		String[] textData = new String[numberOfLines];
		String temp;
		int i=0;
		do{
			temp = textReader.readLine();
			if(temp!=null)
				textData[i]=temp;
			i++;
		}while (temp != null);
	    return textData;
	}
	//convert text to pdf
	private static String text_to_pdf(String[] body_text,String file_name) throws IOException, DocumentException{
		File f1 = text_to_html(body_text,file_name);
		String absolutePath = f1.getAbsolutePath();
		String filePath = absolutePath.substring(0,absolutePath.lastIndexOf(File.separator));
		filePath = filePath + "\\" + file_name + ".pdf";
		html_to_pdf(file_name);
		return filePath;
	}
	//convert html to pdf 
		private static void html_to_pdf(String file_name) throws DocumentException, IOException{
	        String File_To_Convert = file_name + "_bg.html";
	        String url = new File(File_To_Convert).toURI().toURL().toString();
	        System.out.println(""+url);
	        String HTML_TO_PDF = file_name + "_bg.pdf";
	        OutputStream os = new FileOutputStream(HTML_TO_PDF);       
	        ITextRenderer renderer = new ITextRenderer();
	        renderer.setDocument(url);      
	        renderer.layout();
	        renderer.createPDF(os);        
	        os.close();
		}
	
	//get current time
	public static String getCurrentTime(){
		Calendar cal = Calendar.getInstance();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy.MM.dd 'at' hh:mm:ss z");
		return sdf.format(cal.getTime());
	}
}
