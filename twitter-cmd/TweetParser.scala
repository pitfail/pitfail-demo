
package pitfail

import scala.collection.immutable.ListMap

class Amount {
}


abstract class ParseResult()
abstract class MarketAction() extends ParseResult
case class BuyAction (stock: String, amount: Amount) extends MarketAction
case class SellAction(stock: String, amount: Amount) extends MarketAction
case class HackAction(stock: String) extends MarketAction

abstract class BadParse() extends ParseResult
case class NonDirectedTweet () extends BadParse

object TweetParser {
  def apply(s: String) = parse(s)

  def h_groupWithIx[A](as: List[A], bs: Map[A, (Int, List[Int])], curr_pos: Int)
  : Map[A, (Int, List[Int])] = {
    if ((as size) == 0) {
      bs
    } else {
      val a = as head
      val b @ (ct, poses) = bs getOrElse(a, { (0, List()) })
      val new_bs = bs + ((a, (ct + 1, curr_pos :: poses)))
      h_groupWithIx(as tail, new_bs, curr_pos + 1)
    }
  }

  def groupWithIx[A](as: List[A]) : Map[A, (Int, List[Int])] = {
    h_groupWithIx(as, Map[A, (Int, List[Int])](), 0)
  }

  /* Determine the number of times a particular word is mentioned in the tweet. */
  def countWords(tweet: List[String]): ListMap[String, Int] = {
    val gs = tweet groupBy identity
    gs.foldLeft(ListMap[String, Int]())({ (b: ListMap[String, Int], a: (String, List[String]) ) =>
      b + ((a._1, a._2.size))
    })
  }

  def parse(tweet: String) : Option[MarketAction] = {
    val app_user = "pitfail"
    val words = tweet.split("\\s").toList

    if (!words.contains("@pitfail")) {
      return None
    }

    val word_ct = countWords(words)

    val hashes  = word_ct filterKeys (_.startsWith("#"))

    val no_hash = (hashes size) == 0


    /* "@pitfail #buy $100 of MSFT */
    val Simple = """#(\S)+ \$(\d)+ of (\S)+""".r

    /* Things to identify:
     * - the type of action
     * - the involved stock
     *
     * for buy / sell:
     * - amount (some number)
     * - is that in dollars or stocks? (or a percent?)
     */

     return None
  }

}
