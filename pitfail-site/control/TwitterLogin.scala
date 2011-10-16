
package code
package control

import lib.Sugar._

import net.liftweb.{common, http, openid, util, json}
import common._
import util._
import http._
import json._

// For OAuth
// https://github.com/fernandezpablo85/scribe-java/wiki/getting-started
import org.scribe.{builder, model, oauth}
import builder.ServiceBuilder
import builder.api.TwitterApi
import model.{Token, Verifier, OAuthRequest, Verb, Response}
import oauth.OAuthService
import collection.JavaConversions._

// Secretz
// These are not in the repo
import keys.{Twitter => TwitterKeys}

// Dispatch HTTP library
// TODO: Use this

object TwitterLogin extends Loggable
{
    // Dispatcher for Twitter-related requests.
    // These need to be registered in Boot.scala
    def dispatchPF: LiftRules.DispatchPF =
        NamedPF("Twitter login callback") {
            case req@ Req("twitter"::"login"::Nil,    "", _) => twitterLogin(req) _
            case req@ Req("twitter"::"callback"::Nil, "", _) => twitterCallback(req) _
        }
    
    // ------------------------------------------------------------------
    // Accessors for things
    
    def authenticated_? = (accessToken: Option[Token]) match {
        case Some(_) => true
        case _ => false
    }
    
    def asAuth: Option[Auth] =
        for {
            accessToken <- accessToken
        } yield Auth(accessToken)
    
    case class Auth(accessToken: Token) {
        def sign: (String, String) =
            (accessToken.getToken, accessToken.getSecret)
    }
    
    // ------------------------------------------------------------------
    
    // Called when the user clicks on "Login with Twitter"
    protected def twitterLogin(req: Req)(): Box[LiftResponse] = {
        logger info "Beginnig login with Twitter"
        
        // Get a token for this login
        // TODO: I suppose in theory this can fail and we should handle that?
        val token = oauth.getRequestToken()
        requestToken := Some(token)
        
        // Save where to go back to
        returnTo := S.referer openOr "/"
        
        // TODO: There has *got* to be a better way to do this
        val callbackURL =  "http://" + S.hostName + "/twitter/callback"
        
        // And send them to Twitter
        //val redirectURL: String = oauth.getAuthorizationUrl(token,
        //    Map("callback_url" -> callbackURL)
        //)
        val redirectURL: String = oauth.getAuthorizationUrl(token)
        logger info ("Redirecting to " ++ redirectURL)
        Full(RedirectResponse(redirectURL))
    }
    
    // Redirected here after the user logs in
    protected def twitterCallback(req: Req)(): Box[LiftResponse] =
        // TODO: What happens if the URL doesn't have the right query params?
        req.params get "oauth_verifier" match {
            case Some(v::Nil)  => doVerifier(v)
            case _             => throw new IllegalStateException("Figure out later")
        }
    
    protected def doVerifier(verifierText: String): Box[LiftResponse] = {
        val verifier = new Verifier(verifierText)
        logger info ("Got verifier " ++ (verifier toString))
        
        val accessToken_ = (requestToken: Option[Token]) match {
            case Some(token) => oauth.getAccessToken(token, verifier)
            case _  =>
                throw new IllegalStateException("Lost the request token ;(")
        }
        accessToken := Some(accessToken_)
        logger info ("Got access token " ++ (accessToken toString))
        
        // Now we have our credentials. We want to find out who the user is.
        val req = new OAuthRequest(Verb.GET,
            "http://api.twitter.com/1/account/verify_credentials.json")
        oauth.signRequest(accessToken_, req)
        val res = req.send()
        
        // Extract the user's Twitter username
        val username: String = (
            (for {
                JObject(node) <- parse(res.getBody)
                JField("screen_name", JString(username)) <- node
            } yield username)
        
            match {
                case name::_ => name
                case Nil =>
                    throw new IllegalStateException(
                        "Couldn't find screen_name from Twitter")
            }
        )
        logger info ("Verified user as " ++ username)
        
        LoginManager.loginAsTwitter(username)
        
        // And redirect back to where they were before
        Full(RedirectResponse(returnTo))
    }
    
    // Where to go back to after logging in
    protected object returnTo extends SessionVar[String]("/")
    
    // Request token used to obtain access token
    protected object requestToken extends SessionVar[Option[Token]](None)
    
    // Access token for API calls
    protected object accessToken extends SessionVar[Option[Token]](None)
    
    protected val oauth: OAuthService = new ServiceBuilder()
        .provider(classOf[TwitterApi])
        .apiKey(TwitterKeys.consumerKey)
        .apiSecret(TwitterKeys.consumerSecret)
        .callback("http://ellbur:8080/twitter/callback")
        .build()
}

