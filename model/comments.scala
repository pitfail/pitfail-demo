
package model

import org.joda.time.DateTime

trait CommentSchema {
    self: NewsSchema with UserSchema with DBMagic with SchemaErrors =>
    
    implicit val eventComments = table[EventComment]
        
    // Model tables
    
    case class EventComment(
            id:    Key = nextID,
            event: NewsEvent,
            by:    User,
            text:  String,
            when:  DateTime
        )
        extends KL
    
    object EventComment {
        def byID(id: Key) = eventComments lookup id getOrElse (throw NoSuchComment)
    }
        
    trait UserWithComments {
        self: User =>
        
        def userPostComment(event: NewsEvent, text: String) = editDB(postComment(event, text))
        
        private[model] def postComment(event: NewsEvent, text: String) =
            EventComment(event=event, by=this, text=text, when=new DateTime).insert
    }
    
    trait NewsEventWithComments {
        self: NewsEvent =>
        
        def comments = eventComments filter (_.event ~~ this)
        def numComments = comments.length
        
        def userPostAnonymously(text: String) = editDB {
            for {
                user <- User ensure "Anonymous"
                _ <- user postComment (this, text)
            }
            yield ()
        }
    }
}

