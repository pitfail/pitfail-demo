
PitFail -- the fastest way to fail in the pit
=============================================

Building
~~~~~~~~

**Note**: It will not build without *API keys* (see below).

::
    
    ./sbt '~jetty-run'

Should

1. Fetch all dependencies (including Scala 2.9.1 itself)
2. Build all source files
3. Start the web server (Jetty) on port ``8080``.

Now you can go to::

    http://localhost:8080/
 
and see the site.

Navigating
~~~~~~~~~~

Build configuration
-------------------

As I write this, the build configuration is in ``./build.sbt``.

Source code
-----------

Curiously, the directory called ``src`` does *not* contain source code! It
contains a symlink to ``./pitfail-site/webapp`` and is here because I don't
know how to tell the web-plugin where the HTML files actually are.

Actual source code resides in:

``pitfail-site``
    This is the code for the web site.

``stockdata``
    This fetches data from Yahoo.

``matteform``
    This is some form (as in web form) code I extracted into a separate project
    because it felt like too much...

Keys!
~~~~~

You need the API keys which go in ``./pitfail-site/keys/``. Ask me for them.

