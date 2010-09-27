#!/bin/sh

scalac src/alaska/* -d bin
scala -classpath bin alaska.Everest
