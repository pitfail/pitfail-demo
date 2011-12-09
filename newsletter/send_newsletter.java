package newsletter;

import java.io.File;
import java.io.IOException;

import javax.mail.MessagingException;
import javax.mail.internet.AddressException;

import com.lowagie.text.DocumentException;

public class send_newsletter {
	public static void send_newsletter(String userID) throws IOException, DocumentException, AddressException, MessagingException{	
		
		String to_bg = "brian.goodacre@gmail.com"; //update this 
		String subject_bg = "Pitfail Portfolio Update"+create_newsletter.getCurrentTime();;
		String body_bg="See Attachment for Update";
		File attachment_bg = create_newsletter.create_newsletter(userID); //need person to send it to
		send_email.send_email(to_bg,subject_bg,body_bg,attachment_bg,"update.html");
	}
}
