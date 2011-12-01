package stockdata

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.ListMap

class HttpQueryServiceTests extends Spec with ShouldMatchers {
  describe("buildQuery") {
    it("creates key-value pair") {
      val queryString = HttpQueryService.buildQuery(Map("a" -> "b"), "ASCII")
      queryString should equal ("a=b")
    }

    it("delimits key-value pairs") {
      val queryString = HttpQueryService.buildQuery(ListMap("a" -> "b", "c" -> "d"), "ASCII")
      queryString should equal ("a=b&c=d")
    }

    it("encodes keys") {
      val queryString = HttpQueryService.buildQuery(Map("a b" -> "c"), "ASCII")
      queryString should equal ("a+b=c")
    }

    it("encodes values") {
      val queryString = HttpQueryService.buildQuery(Map("a" -> "b c"), "ASCII")
      queryString should equal ("a=b+c")
    }
  }

  describe("parseQuery") {
    it("parses key-value pair") {
      val params = HttpQueryService.parseQuery("a=b", "ASCII")
      params should equal (Map("a" -> "b"))
    }

    it("parses delimited key-value pairs") {
      val params = HttpQueryService.parseQuery("a=b&c=d", "ASCII")
      params should equal (ListMap("a" -> "b", "c" -> "d"))
    }

    it("decodes keys") {
      val params = HttpQueryService.parseQuery("a+b=c", "ASCII")
      params should equal (Map("a b" -> "c"))
    }

    it("decodes values") {
      val params = HttpQueryService.parseQuery("a=b+c", "ASCII")
      params should equal (Map("a" -> "b c"))
    }
  }
}

// vim: set ts=2 sw=2 et:
