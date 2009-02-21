(ns lambinator.demo.basic
  (:use lambinator.demo.functionalgraphics
     lambinator.demo.util
     lambinator.ui
     lambinator.ui.gl
     lambinator.ui.inspector)
  (:import (javax.media.opengl GL DebugGL)
     (javax.media.opengl.glu GLU)))

(defn render-frame [drawable]
  (println "time.............")
  (let [#^GL gl (DebugGL. (.getGL drawable))
        #^GLU glu (GLU.)]
    (doto gl
      (.glUseProgram 0 )
      (.glClearColor 0.05 0.05 0.1 1.0)
      (.glClear GL/GL_COLOR_BUFFER_BIT)
      (.glShadeModel GL/GL_SMOOTH)
      (.glDisable GL/GL_BLEND)
      (.glMatrixMode GL/GL_PROJECTION)
      (.glLoadIdentity)
      ;(. glu gluOrtho2D minx maxx miny maxy)
      (.glMatrixMode GL/GL_MODELVIEW)
      (.glLoadIdentity)
      (.glBegin GL/GL_TRIANGLES) ;						// Drawing Using Triangles
      (.glVertex3f 0.0 1.0 0.0)  ;				// Top
      (.glVertex3f -1.0 -1.0 0.0)  ;				// Bottom Left
      (.glVertex3f 1.0 -1.0 0.0)  ;				// Bottom Right
      (.glEnd))))

(defn create-drawable-fn[demo-data-ref]
  (let [num-samples @(@demo-data-ref :num-samples-ref)
        local-drawable #(render-frame %)
        frame (@demo-data-ref :frame)
        uigl (ui-get-gl-window-data (@demo-data-ref :frame))
        drawable-fn (if (= num-samples :none)
                      local-drawable
                      (do
                        (dmut-create-multisample-render-data @demo-data-ref)
                        (dmut-create-aa-drawable-fn @demo-data-ref local-drawable)))]
    (uii-setup-inspector-panel (frame :inspector-pane) [(@demo-data-ref :inspector-item)])
    (uigl-set-render-fn uigl drawable-fn)
    (uigl-repaint uigl)))

(defn create-basic-demo [frame retval]
  (let [uigl    (ui-get-gl-window-data frame)
        ms-data (dmut-create-multisample-data 
                  frame 
                  #(create-drawable-fn retval))]
    (dosync (ref-set retval ms-data))
    ;    (uigl-disable-fps-animator uigl)
    (uigl-set-fps-animator uigl 5)
    (create-drawable-fn retval)))

(defn destroy-basic-demo
  "Release the resources associated with this demo"
  [demo-data-ref]
  (dmut-delete-multisample-render-data @demo-data-ref))

