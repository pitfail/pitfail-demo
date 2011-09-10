name         := "Pitfail"

version      := "1.0"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "joda-time"      % "joda-time"       % "2.0",
  "org.joda"       % "joda-convert"    % "1.0",
  "net.liftweb"    % "lift-json_2.9.0" % "2.4-M3",
  "net.databinder" % "dispatch-http_2.9.0"  % "0.8.5",
  "org.scalatest"  % "scalatest_2.9.0" % "1.6.1" % "test"
)

