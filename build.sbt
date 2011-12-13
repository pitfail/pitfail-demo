
name := "pitfail-demo"

scalaVersion := "2.9.1"

target <<= baseDirectory(_ / ".target")

scalaSource in Compile <<= baseDirectory

scalaSource in Test    <<= baseDirectory

sourceDirectories in Compile ~= { srcDirs => srcDirs filter(!_.getAbsolutePath.endsWith("src/main/java")) }

includeFilter in Compile in unmanagedSources <<=
	(includeFilter in unmanagedSources, baseDirectory) { (ff, baseDir) =>
		ff && new SimpleFileFilter({ file =>
			val path = Path.relativizeFile(baseDir, file).get.getPath
			(
                   ! path.contains("test" + java.io.File.separator)
                && ! path.startsWith("project") && ! path.startsWith("android")
            )
		})
	}

includeFilter in Test in unmanagedSources <<=
	(includeFilter in unmanagedSources, baseDirectory) { (ff, baseDir) =>
		ff && new SimpleFileFilter({ file =>
			val path = Path.relativizeFile(baseDir, file).get.getPath
			val test = "test" + java.io.File.separator
			path.contains(test) && ! path.startsWith("project") && ! path.startsWith("android")
		})
	}

traceLevel in Runtime := 5

traceLevel in Compile := 0

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-Xexperimental"
)

// For scalaz
resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies ++= Seq(
    "joda-time"        %  "joda-time"           % "2.0",
    "org.joda"         %  "joda-convert"        % "1.0",
    "net.databinder"   % "dispatch-core_2.9.1"       % "0.8.5",
    "net.databinder"   % "dispatch-http-json_2.9.1"  % "0.8.5",
    "net.databinder"   % "dispatch-oauth_2.9.1"      % "0.8.5",
    "net.databinder"   % "dispatch-nio_2.9.1"        % "0.8.5",
    "net.liftweb"      % "lift-webkit_2.9.1"         % "2.4-M4",
    "net.liftweb"      % "lift-openid_2.9.1"         % "2.4-M4",
    "net.liftweb"      % "lift-json-ext_2.9.1"       % "2.4-M4",
    "net.liftweb"      % "lift-widgets_2.9.1"        % "2.4-M4" % "compile->default",
    "org.mortbay.jetty" % "jetty"             % "6.1.22" % "jetty",
    "org.slf4j"        % "slf4j-log4j12"         % "1.6.1",
    "com.h2database"   % "h2"                   % "1.3.159",
    "org.squeryl"      % "squeryl_2.9.1"             % "0.9.4",
    "org.scalaz"       % "scalaz-core_2.9.1"         % "6.0.3",
    "junit"            % "junit"                % "4.5"    % "test->default",
    "org.scala-tools.testing" % "specs_2.9.1"      % "1.6.9"  % "test->default",
    "org.scalatest"    % "scalatest_2.9.1"           % "1.6.1"  % "test",
    "javax.servlet"    %  "servlet-api"        % "2.4",
    "com.google.code.gson" % "gson"             % "1.7.1",
    "org.scala-tools.time" % "time_2.9.1" % "0.5"
)

// For the Jetty server
seq(com.github.siasia.WebPlugin.webSettings: _*)

// vim:syn=scala
