import sbt._
object MyApp extends Build
{
  lazy val root =
    Project("", file(".")) dependsOn(dispatchTwitter)
  lazy val dispatchTwitter =
    uri("git://github.com/pitfail/dispatch-twitter.git#v1")
}
