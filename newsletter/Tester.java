package newsletter;

import javax.mail.MessagingException;
import javax.mail.internet.AddressException;
import java.io.*;


import com.itextpdf.text.*;
import com.itextpdf.text.pdf.*;
import com.lowagie.text.DocumentException;

//Testing the interface for sending emails :)
public class Tester {
	public static void main(String argc[]) throws AddressException, MessagingException, IOException, DocumentException{
		String to_bg="brian.goodacre@gmail.com";
		String subject_bg="Test email_ Pitfail - " + create_newsletter.getCurrentTime();
		String body_bg="See Attachment for Newsletter";
		File attachment_bg =create_newsletter.create_newsletter("q33q3");
		send_email.send_email(to_bg,subject_bg,body_bg,attachment_bg,"update.html");
		
		
	}
}
