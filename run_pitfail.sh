#! /bin/sh


cd $(dirname "$0")/server && ./sbt '~jetty-run'

