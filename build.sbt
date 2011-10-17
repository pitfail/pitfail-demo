
name := "pitfail-demo"

scalaVersion := "2.9.1"

// This is where you'd set the log level. Believe it or not this is
// not very useful. Defaults to Info.
// logLevel := Level.Debug

scalaSource in Compile <<= baseDirectory(identity)

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-explaintypes"
)

// For scalaz
resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.0",
    "org.joda" % "joda-convert" % "1.0",
    "net.databinder" % "dispatch-core_2.9.0-1"       % "0.8.5",
    "net.databinder" % "dispatch-http-json_2.9.0-1"  % "0.8.5",
    "net.databinder" % "dispatch-lift-json_2.9.0-1"  % "0.8.5",
    "net.databinder" % "dispatch-oauth_2.9.0-1"      % "0.8.5",
    "net.databinder" % "dispatch-nio_2.9.0-1"        % "0.8.5",
    "net.liftweb" % "lift-webkit_2.9.0" % "2.4-M3" % "compile->default",
    "net.liftweb" % "lift-openid_2.9.0" % "2.4-M3",
    "net.liftweb" % "lift-json-ext_2.9.0" % "2.4-M3",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "jetty",
    "junit" % "junit" % "4.5" % "test->default",
    "org.scala-tools.testing" % "specs_2.9.0" % "1.6.8" % "test->default",
    "org.slf4j" % "slf4j-simple" % "1.6.1",
    "com.h2database" % "h2" % "1.3.159",
    "org.squeryl" % "squeryl_2.9.0" % "0.9.4",
    "org.scalaz" %% "scalaz-core" % "6.0.3"
)

// For the Jetty server
seq(com.github.siasia.WebPlugin.webSettings: _*)

