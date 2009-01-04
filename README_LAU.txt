The wave demo requires you have installed jogl already.  I don't include that in
the system.

I do include mydoggy (which is debatable since it doesn't look exactly right).
The system expects that you have clojure, clojure-contrib, and jogl in your
classpath or that your java command will find them as necessary.

Given you can, from the clojure repl, launch clojure and (import
'(javax.media.opengl GL)), then you may be able to the demo to run.  Simple
type: ./run_wave_demo.

It setups up the java classpath as follows:
-cp $CLASSPATH:lambinator.jar:lib-jars:.

Let me know how it goes...  
cnuernber@gmail.com

Chris
