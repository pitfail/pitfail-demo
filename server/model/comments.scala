/* written by: Owen Healy
 * written by: Cody Schafer <cpschafer@gmail.com>
 */

package model

import org.joda.time.DateTime
import spser._

trait CommentSchema extends Schema {
    self: NewsSchema with UserSchema with DBMagic with SchemaErrors =>
    
    implicit val ecCon = EventComment.apply _
        
    implicit val eventComments: Table[EventComment] = table[EventComment]
    
    abstract override def tables = eventComments :: super.tables
        
    // Model tables
    
    case class EventComment(
            id:    Key = nextID,
            event: Link[NewsEvent],
            by:    Link[User],
            text:  String,
            when:  DateTime
        )
        extends KL
    
    object EventComment {
        def byID(id: Key) = ((eventComments where ('id~=~id)).headOption
            getOrElse (throw NoSuchComment))
    }
       


    trait UserWithComments {
        self: User =>
        
        // ref_494
        def userPostComment(event: NewsEvent, text: String) = editDB(postComment(event, text))
        
        private[model] def postComment(event: NewsEvent, text: String) =
            EventComment(event=event, by=this, text=text, when=new DateTime).insert
    }
    
    trait NewsEventWithComments {
        self: NewsEvent =>
        
        // ref_449
        def comments = eventComments where ('event ~=~ this) toList
        def numComments = comments.length
            
        def userPostAnonymously(text: String) = editDB {
            val user = User userEnsure "Anonymous"
            for {
                _ <- user postComment (this, text)
            }
            yield ()
        }
    }
}

