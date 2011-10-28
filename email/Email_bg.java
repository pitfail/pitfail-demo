//Brian Goodacre

package email;

import java.util.*;
import javax.mail.*;
import javax.mail.internet.*;
import java.util.Calendar;
import java.text.SimpleDateFormat;

public class Email_bg {
	
	public static void main(String[] args) throws AddressException, MessagingException {
		String to = "sirgiant@gmail.com";
		String subject = "Pitfail Confirmation";
		String body = "This email is confirming the following transcation: "; 
		Email_bg.send_email(to,subject,body);
	}

	private static String getCurrentTime(){
		Calendar cal = Calendar.getInstance();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy.MM.dd 'at' hh:mm:ss z");
		return sdf.format(cal.getTime());
	}
	
	//This function sends emails to user
	public static void send_email(String to_input, String subject_input, String body_input) throws AddressException, MessagingException{
		//Email's From Address:
		String email_from_real = "pitfail@gmx.com";
		
		/*Brian's Information for sending through the Rutgers's Servers*/
		String from = "bgoodacr@eden.rutgers.edu";
		String pass = "Acre2012";
		
		//Server information
		String host = "smtp.rutgers.edu";
		Properties props = System.getProperties();
		props.put("mail.smtp.starttls.enable", "true"); 
		props.put("mail.smtp.host", host);
		props.put("mail.smtp.user", from);
		props.put("mail.smtp.password", pass);
		props.put("mail.smtp.port", "25");
		props.put("mail.smtp.auth", "true");

		//Message Information
		Session session = Session.getDefaultInstance(props, null);
		MimeMessage message = new MimeMessage(session);
		//message.setFrom(new InternetAddress(from));
			//populating the from field
		InternetAddress to_field = new InternetAddress(to_input);
		InternetAddress temp_InternetAddress1 = new InternetAddress("pitfail@pitfail.com");
		InternetAddress temp_InternetAddress2 = new InternetAddress(email_from_real);
		Address[] from_addresses = new Address[2];
		from_addresses[0]=temp_InternetAddress1;
		from_addresses[1]=temp_InternetAddress2;
			//message details
		message.addFrom(from_addresses);
		message.addRecipient(Message.RecipientType.TO, to_field);
		message.addRecipient(Message.RecipientType.TO, new InternetAddress("pitfail@gmx.com"));
		String subject_temp = subject_input + " - " + Email_bg.getCurrentTime();
		message.setSubject(subject_temp);
		message.setText(body_input);
		message.setFrom(from_addresses[1]);

		//Send message
		Transport transport = session.getTransport("smtp");
		transport.connect(host, from, pass);
		transport.sendMessage(message, message.getAllRecipients());
		transport.close();
		
		//Confirmation
		System.out.println("Successful email sent.");
		System.out.println("To: " + to_input);
		System.out.println("From: " + email_from_real);
		System.out.println("Subject: " + subject_temp);
		System.out.println("Body: " + body_input);
		System.out.println("--End--\n");


	}
}
