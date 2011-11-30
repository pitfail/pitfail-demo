
package code
package control

import net.liftweb.{common, http, openid, util}
import common._
import util._
import http._

import openid.{OpenIDVendor, SimpleOpenIDVendor}
import org.openid4java.{consumer, discovery}
import discovery.Identifier
import consumer.VerificationResult

// See
// http://scala-tools.org/mvnsites/liftweb-1.1-M8/lift-modules/lift-openid/
//        scaladocs/net/liftweb/openid/OpenId.scala.html#Some(182)
//
// for the source of this madness.

object OpenIDLogin
    extends SimpleOpenIDVendor
    with Loggable
{
    override def dispatchPF: LiftRules.DispatchPF = NamedPF("Login callback") {
        case req @ Req("openid"::"login"::Nil, "", GetRequest) => sendToProvider _
        case req @ Req("openid"::Nil, "", _) => receiveFromProvider _
    }
    
    // What does this actually return? I have no clue.
    // Yay type inference...
    def sendToProvider() = {
        try {
            // Save the url to redirect back to in session var
            redirectBackTo(S.referer openOr "/")

            // Send to Google
            Full(OpenIDObject.is.authRequest(
                "https://www.google.com/accounts/o8/id",
                "/openid"
            ))
        }
        catch { case err =>
            S.error("Open ID failed: " + err.getMessage())
            logger.error(err.getMessage())
            logger.error(err)
            
            Full(RedirectResponse("/"))
        }
    }
    
    def receiveFromProvider() = {
        def makeResponse(req: Req) = {
            val (uid, verif) = OpenIDObject.is.verifyResponse(req.request)
            
            OpenIDObject onComplete match {
                case Full(f) => Full(f(uid, Full(verif), Empty))
                case _ =>
                    postLogin(uid, verif)
                    val returnURL = redirectBackTo.is
                    Full(RedirectResponse(returnURL, S.responseCookies:_*))
            }
        }
        
        // I don't know why we're using a for comprehension
        for (
             req      <- S.request;
             response <- makeResponse(req)
        ) yield(response)
    }
    
    override def postLogin(
        uid: Box[Identifier],
        ver: VerificationResult
    ) {
        uid match {
            case Full(uid) =>
                LoginManager.loginAsOpenID(uid.getIdentifier)
                
            case _  => LoginManager.logout()
        }
    }
    
    // URL to take user back to after logging them in
    private object redirectBackTo extends SessionVar[String]("/")
}

