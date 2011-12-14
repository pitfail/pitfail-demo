
// Written by: Owen Healy

package stockdata

import org.joda.time.{DateTime,Duration}
import scala.collection.mutable.{Map => MMap}

class CacheMap[K,V](timeout: Duration) {
    def put(key: K, value: V) = {
        map(key) = Entry(value, new DateTime)
        value
    }
    
    def get(key: K)(fetch: => V) = {
        map get key match {
            case Some(Entry(value, time))
                if (!expired(time)) => value
            
            case _ => put(key, fetch)
        }
    }
    
    private[this] val map = MMap[K,Entry]()
    private[this] case class Entry(value: V, time: DateTime)
    
    def expired(time: DateTime) = timeout.compareTo(
        new Duration(time, new DateTime)) < 0
}

