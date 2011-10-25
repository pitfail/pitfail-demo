
package code
package snippet

import net.liftweb.{common, http, util}
import common._
import util._
import http.{js}
import js._
import JsCmds._
import JE._
import Helpers._
import scala.xml.NodeSeq

import code.control.{LoginManager}

// The little box at the top of the sidebar that shows whether you're logged
// in.

object LoginStatus extends Loggable {
    import LoginManager.{loggedIn_?, currentLogin}
    
    def ifLoggedIn(in: NodeSeq): NodeSeq = {
        if (loggedIn_?) in
        else Nil
    }
    
    def ifLoggedOut(in: NodeSeq): NodeSeq = {
        if (!loggedIn_?) in
        else Nil
    }
    
    def username(in: NodeSeq): NodeSeq = {
        val name = currentLogin match {
            case Some(n) => n
            case _  => "Nobody"
        }
        <a href="/user">{name}</a>
    }
}

