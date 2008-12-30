#!/bin/sh -e
ant clean
ant -Dclojure.jar=/Users/chrisn/dev/clojure/trunk/clojure.jar\
 -Dqt_jar=/Users/chrisn/dev/qtjambi-mac-gpl-4.4.3_01/qtjambi-4.4.3_01.jar\
 -Dqt_gcc_jar=/Users/chrisn/dev/qtjambi-mac-gpl-4.4.3_01/qtjambi-macosx-gcc-4.4.3_01.jar\
 -Djogl_jar=/Users/chrisn/dev/jogl-1.1.1-macosx-universal/lib/jogl.jar\
 -Dclojure_contrib_jar=/Users/chrisn/dev/clojure-contrib/trunk/clojure-contrib.jar\
 -Dgluegen_jar=/Users/chrisn/dev/jogl-1.1.1-macosx-universal/lib/gluegen-rt.jar
