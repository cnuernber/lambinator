(ns lambinator.lwjgl
  (:use (lambinator util fs))
  (:import (org.lwjgl.opengl AWTGLCanvas EXTFramebufferObject)
	   (javax.swing JFrame SwingUtilities JLabel)))

(defn gl-render [canvas]
  (println "Attempting to allocate fbos")
  (let [tex-buf (util-direct-int-buffer 1)
	rb-buf (util-direct-int-buffer 1)
	fbo-buf (util-direct-int-buffer 1)]
    ))

(defn create-canvas-proxy []
  (proxy [AWTGLCanvas] []
    (paintGL [] (gl-render this))))
	     

(defn create-view []
  (let [canvas-ref (ref nil)]
    (SwingUtilities/invokeLater 
     (fn []
       (let [frame (JFrame.)
	     canvas (create-canvas-proxy)
	     test-label (JLabel. "TestLabel")
	     content-pane (.getContentPane frame)]
	 (.setSize frame 200 200)
	 (.add content-pane canvas)
	 (.setVisible canvas true)
	 (.show frame)
	 (dosync (ref-set canvas-ref canvas)))))
    canvas-ref))