#!/bin/sh -e
ant -Dclojure.jar=../../clojure/trunk/clojure.jar\
 -Dqt_jar=../../qtjambi-mac-gpl-4.4.3_01/qtjambi-4.4.3_01.jar\
 -Dqt_gcc_jar=../../qtjambi-mac-gpl-4.4.3_01/qtjambi-macosx-gcc-4.4.3_01.jar\
 -Djogl_jar=../../jogl-1.1.1-macosx-universal/lib/jogl.jar
