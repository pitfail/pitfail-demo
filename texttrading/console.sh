#!/bin/sh

sbt -Djline.terminal=jline.UnsupportedTerminal 'run-main texttrading.test.ConsoleTest'

