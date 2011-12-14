
server/   -- Main application, provides the database & model, twitter
	     interaction, and website. Build with './sbt' in the server
	     directory.
android/  -- Android application (runs on Android devices).
facebook/ -- Facebook application (seperate server program that interacts with
             the main server). Build with eclipse.

To Run the Server:

$ cd server
$ ./sbt \~jetty-run

