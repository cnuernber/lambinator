#!/bin/sh -e
#you *must* have CLASSPATH setup for this to work
#or your compiler must be able to find clojure somehow
#without classpath.
#furthermore, you need 
#JOGL https://jogl.dev.java.net/
ant clean
ant -Dmydoggy_jar=lib/mydoggy-api-1.4.2.jar\
 -Dmydoggy_plaf_jar=lib/mydoggy-plaf-1.4.2.jar\
 -Dtable_layout_jar=lib/TableLayout-20050920.jar\
 -Dclasspath=$CLASSPATH\
