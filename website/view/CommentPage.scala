
package code
package snippet

import net.liftweb.{common, http, util}
import common.{Loggable}
import util.{Helpers}
import scala.xml.{NodeSeq}
import http._
import js._
import JsCmds._
import JE._
import Helpers._

import formats._
import intform._
import errors._

import model.schema._

object commentPage {
    def apply(ev: NewsEvent): NodeSeq = {
        
        // -----------------------------------------------
        // Overall layout
        
        lazy val main = Refreshable(
            <div>
                <h3 class="comments">Comments:</h3>
                {
                    if (ev.comments isEmpty) <p>None</p>
                    else showComments
                }
                {postForm.render}
            </div>
        )
        
        // -----------------------------------------------
        // Comments
        
        def showComments = ev.comments map showComment _
        
        def showComment(c: EventComment) =
            <div class="comment">
                <span class="commentDate">{c.when.toNearbyString}</span>
                <span class="commentAuthor">{UserLink(c.by)}:</span>
                <span class="commentText">{c.text}</span>
            </div>
            
        // -----------------------------------------------
        // Posting
            
        lazy val postForm: Form[String] = Form(identity[String],
            (
                commentField
            ),
            <h3>Add comment:</h3>
            <p>{commentField.main}</p>
            <p>{postSubmit.main} {postSubmit.errors}</p>
        )
        
        lazy val commentField = TextAreaField()
        
        lazy val postSubmit = Submit(postForm, "Post") { case text =>
            import control.LoginManager._
            
            try {
                currentUser.userPostComment(ev, text)
            }
            catch {
                case NotLoggedIn => ev.userPostAnonymously(text)
            }
            
            main.refresh()
        }
        
        main.render
    }
}

