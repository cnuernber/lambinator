#!/bin/sh -e
ant clean
ant -Dclojure.jar=/Users/chrisn/dev/clojure/trunk/clojure.jar\
 -Djogl_jar=/Users/chrisn/dev/jogl-1.1.1-macosx-universal/lib/jogl.jar\
 -Dgluegen_jar=/Users/chrisn/dev/jogl-1.1.1-macosx-universal/lib/gluegen-rt.jar\
 -Dclojure_contrib_jar=/Users/chrisn/dev/clojure-contrib/trunk/clojure-contrib.jar\
 -Dmydoggy_jar=lib/mydoggy-api-1.4.2.jar\
 -Dmydoggy_plaf_jar=lib/mydoggy-plaf-1.4.2.jar\
 -Dtable_layout_jar=lib/TableLayout-20050920.jar\
 -Dclasspath=$CLASSPATH\
