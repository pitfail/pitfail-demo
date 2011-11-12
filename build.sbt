
name := "pitfail-demo"

scalaVersion := "2.9.1"

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.1")

target <<= baseDirectory(_ / ".target")

// This is where you'd set the log level. Believe it or not this is
// not very useful. Defaults to Info.
// logLevel := Level.Debug

scalaSource in Compile <<= baseDirectory

scalaSource in Test    <<= baseDirectory

sourceDirectories in Compile ~= { srcDirs => srcDirs filter(!_.getAbsolutePath.endsWith("src/main/java")) }

includeFilter in Compile in unmanagedSources <<=
	(includeFilter in unmanagedSources, baseDirectory) { (ff, baseDir) =>
		ff && new SimpleFileFilter({ file =>
			val path = Path.relativizeFile(baseDir, file).get.getPath
			(
                   ! path.contains("test" + java.io.File.separator)
                && ! path.startsWith("project")
            )
		})
	}

includeFilter in Test in unmanagedSources <<=
	(includeFilter in unmanagedSources, baseDirectory) { (ff, baseDir) =>
		ff && new SimpleFileFilter({ file =>
			val path = Path.relativizeFile(baseDir, file).get.getPath
			val test = "test" + java.io.File.separator
			path.contains(test) && ! path.startsWith("project")
		})
	}

traceLevel in Runtime := 5

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-P:continuations:enable",
    "-Xexperimental"
)

// For scalaz
resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies ++= Seq(
    "joda-time"        %  "joda-time"           % "2.0",
    "org.joda"         %  "joda-convert"        % "1.0",
    "net.databinder"   %% "dispatch-core"       % "0.8.5",
    "net.databinder"   %% "dispatch-http-json"  % "0.8.5",
    "net.databinder"   %% "dispatch-lift-json"  % "0.8.5",
    "net.databinder"   %% "dispatch-oauth"      % "0.8.5",
    "net.databinder"   %% "dispatch-nio"        % "0.8.5",
    "net.liftweb"      %% "lift-webkit"         % "2.4-M4",
    "net.liftweb"      %% "lift-openid"         % "2.4-M4",
    "net.liftweb"      %% "lift-json-ext"       % "2.4-M4",
    "org.mortbay.jetty" % "jetty"             % "6.1.22" % "jetty",
    //"org.slf4j"        % "slf4j-simple"         % "1.6.1",
    "org.slf4j"        % "slf4j-log4j12"         % "1.6.1",
    //"com.h2database"   % "h2"                   % "1.3.159",
    //"org.apache.derby" % "derby"                % "10.8.1.2",
    //"org.xerial"       % "sqlite-jdbc"          % "3.7.2",
    "org.squeryl"      %% "squeryl"             % "0.9.4",
    "org.scalaz"       %% "scalaz-core"         % "6.0.3",
    "junit"            % "junit"                % "4.5"    % "test->default",
    "org.scala-tools.testing" %% "specs"      % "1.6.9"  % "test->default",
    "org.scalatest"    %% "scalatest"           % "1.6.1"  % "test",
    "javax.servlet"    %  "servlet-api"        % "2.4",
    "com.google.code.gson" % "gson"             % "1.7.1"
)

// For the Jetty server
seq(com.github.siasia.WebPlugin.webSettings: _*)

// vim:syn=scala
