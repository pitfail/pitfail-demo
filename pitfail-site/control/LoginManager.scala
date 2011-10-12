
package code
package control

import lib.Sugar._

import net.liftweb.{common, http, openid, util, json}
import common._
import util._
import http._
import http.{SessionVar}
import model.Schema._

object LoginManager {
    
    case class NotLoggedIn() extends RuntimeException
    
    def loggedIn_? = currentLogin match {
        case Some(_) => true
        case _ => false
    }
    
    def loginAsOpenID(id: String) {
        throw new IllegalStateException("Not supported")
    }
    
    def loginAsTwitter(name: String) {
        import model.Schema
        
        _currentLogin := Some(name)
        Schema.touchUser(name)
    }
    
    def logout() {
        _currentLogin := None
    }
    
    def currentLogin: Option[String] = _currentLogin.is
    
    def withUser(op: User => Unit) {
        currentLogin match {
            case Some(name) =>
                byUsername(name) match {
                    case Some(user) => op(user)
                    case _ => throw new IllegalStateException("Invalid user")
                }
            case _ => throw NotLoggedIn()
        }
    }
    
    // Who we're logged in as
    object _currentLogin extends SessionVar[Option[String]](Some("ellbur_k_a"))
}

