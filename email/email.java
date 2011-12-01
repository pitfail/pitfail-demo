package email;

import java.util.*;
import javax.mail.*;
import javax.mail.internet.*;

public class email{
        public static void main(String[] args) throws AddressException, MessagingException {
		String to = "sirgiant@gmail.com";
		String subject = "Pitfail Confirmation";
		String body = "This email is confirming the following transcation: "; 
		Email_bg.send_email(to,subject,body);
	}
}
