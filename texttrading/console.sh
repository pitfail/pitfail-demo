#!/bin/sh

(
	cd $(dirname "$0")/../
	sbt -Djline.terminal=jline.UnsupportedTerminal 'run-main texttrading.test.ConsoleTest'
)

