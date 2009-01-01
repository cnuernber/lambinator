#!/bin/sh -e
ant clean
ant -Dclojure.jar=/Users/chrisn/dev/clojure/trunk/clojure.jar\
 -Dqt_jar=/Users/chrisn/dev/qtjambi-mac-gpl-4.4.3_01/qtjambi-4.4.3_01.jar\
 -Dqt_gcc_jar=/Users/chrisn/dev/qtjambi-mac-gpl-4.4.3_01/qtjambi-macosx-gcc-4.4.3_01.jar\
 -Djogl_jar=/Users/chrisn/dev/jogl-1.1.1-macosx-universal/lib/jogl.jar\
 -Dgluegen_jar=/Users/chrisn/dev/jogl-1.1.1-macosx-universal/lib/gluegen-rt.jar\
 -Dclojure_contrib_jar=/Users/chrisn/dev/clojure-contrib/trunk/clojure-contrib.jar\
 -Dmydoggy_jar=/Users/chrisn/dev/mydoggy-1/lib/mydoggy-api-1.4.2.jar\
 -Dmydoggy_plaf_jar=/Users/chrisn/dev/mydoggy-1/lib/mydoggy-plaf-1.4.2.jar\
 -Dtable_layout_jar=/Users/chrisn/dev/mydoggy-1/lib/TableLayout-20050920.jar\
