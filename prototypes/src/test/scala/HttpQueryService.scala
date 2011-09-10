package com.github.pitfail

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.ListMap

class HttpQueryServiceTests extends FunSuite with ShouldMatchers {
  test("buildQuery: Creates key-value pair.") {
    val queryString = HttpQueryService.buildQuery(Map("a" -> "b"), "ASCII")
    queryString should equal ("a=b")
  }

  test("buildQuery: Delimits key-value pairs.") {
    val queryString = HttpQueryService.buildQuery(ListMap("a" -> "b", "c" -> "d"), "ASCII")
    queryString should equal ("a=b&c=d")
  }

  test("buildQuery: Encodes keys.") {
    val queryString = HttpQueryService.buildQuery(Map("a b" -> "c"), "ASCII")
    queryString should equal ("a+b=c")
  }

  test("buildQuery: Encodes values.") {
    val queryString = HttpQueryService.buildQuery(Map("a" -> "b c"), "ASCII")
    println(queryString)
    queryString should equal ("a=b+c")
  }

  test("parseQuery: Parses key-value pair.") {
    val params = HttpQueryService.parseQuery("a=b")
    params should equal (Map("a" -> "b"))
  }

  test("parseQuery: Parses delimited key-value pairs.") {
    val params = HttpQueryService.parseQuery("a=b&c=d")
    params should equal (ListMap("a" -> "b", "c" -> "d"))
  }

  test("parseQuery: Decodes keys.") {
    val params = HttpQueryService.parseQuery("a+b=c")
    params should equal (Map("a b" -> "c"))
  }

  test("parseQuery: Decodes values.") {
    val params = HttpQueryService.parseQuery("a=b+c")
    params should equal (Map("a" -> "b c"))
  }
}
