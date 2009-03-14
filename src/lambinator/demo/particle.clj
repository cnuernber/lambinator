(ns lambinator.demo.particle
  (:use lambinator.demo.functionalgraphics
     lambinator.demo.util
     lambinator.ui
     lambinator.ui.gl
     lambinator.ui.inspector)
  (:import 
     (javax.media.opengl GL DebugGL)
     (java.io File)
	   (com.sun.opengl.util.texture TextureIO)
     (javax.media.opengl.glu GLU)))

(def SLOWDOWN 2000.0)
(def ZOOM     40.0)

(def *particles* (ref nil))

(defstruct particle 
           :active :life :fade ; status, life-left, and aging-speed
           :r :g :b     ; color
           :x :y :z     ; position
           :xi :yi :zi  ; direction
           :xg :yg :zg) ; gravity

(comment defn- load-texture [path]
  (let [tex (TextureIO/newTexture )] ))

(defn- rand-fade []
  (/ (rand 100) (+ 1000 0.003)))

(defn- rand-speed []
  (- (rand 500) 250.0))

(defn- make-particles [n]
  (map (fn [i] 
         (struct-map particle 
	   :active true 
	   :life 1.0 
	   :fade (rand-fade)
	   :r 0.5 
	   :b 5.0
	   :g 1.0 ; Just using white for now
	   :x 0.0
	   :y 0.0
	   :z -100.0
	   :xi (rand-speed) 
	   :yi (rand-speed) 
	   :zi (rand-speed)
	   :xg 0.0 
	   :yg -0.8 
	   :zg 0.0)) (range n)))

(defn- update-particles [particles]
  (map (fn [p]
    (let [x (+ (:x p) (/ (:xi p) SLOWDOWN))
          y (+ (:y p) (/ (:yi p) SLOWDOWN))
          z (+ (:z p) (/ (:zi p) SLOWDOWN))
          xi (+ (:xi p) (:xg p))
          yi (+ (:yi p) (:yg p))
          zi (+ (:zi p) (:zg p))
          life (- (:life p) (:fade p))]
      (merge p {:x x :y y :z z
                :xi xi :yi yi :zi zi
                :life life}))) particles))

(defn- draw-particles [#^GL gl particles]
  (doseq [p particles]
    (if (:active p)
      (let [x (:x p)
            y (:y p)
            z (:z p)]
        (doto gl
          ;(.glColor4f (:r p) (:g p) (:b p) (:life p))
          (.glColor3f 0.8 0.8 1.0)
          (.glBegin GL/GL_TRIANGLE_STRIP)
          (.glVertex3f (+ x 0.5) (+ y 0.5) z) ; top-right
          (.glVertex3f (- x 0.5) (+ y 0.5) z) ; top-left
          (.glVertex3f (+ x 0.5) (- y 0.5) z) ; bottom-right
          (.glVertex3f (- x 0.5) (- y 0.5) z)
          (.glEnd)))))) ; bottom-left

(defn- render-frame [drawable]
  (dosync 
    (ref-set *particles* (update-particles @*particles*)))
  (let [#^GL gl (DebugGL. (.getGL drawable))
        #^GLU glu (GLU.)
        width (.getWidth drawable)
        height (.getHeight drawable)]
    (doto gl
      (.glUseProgram 0 )
      (.glClearColor 0.05 0.05 0.1 1.0)
      (.glClear GL/GL_COLOR_BUFFER_BIT)
      (.glShadeModel GL/GL_SMOOTH)
      ;(. gl glPolygonMode GL/GL_FRONT_AND_BACK GL/GL_LINE)
      ;(. gl glPolygonMode GL/GL_FRONT_AND_BACK GL/GL_POINT)
      ;(.glDisable GL/GL_BLEND)
      (.glMatrixMode GL/GL_PROJECTION)
      (.glLoadIdentity))
    (. glu gluPerspective 45  (/ width height) 0.1 100.0)
    (doto gl 
      (.glMatrixMode GL/GL_MODELVIEW)
      (.glLoadIdentity))
    (try 
      (draw-particles gl @*particles*)
      (catch javax.media.opengl.GLException e 
        (do 
	  (.printStackTrace e)
          (println "GL ERROR: " (. glu gluErrorString (. gl glErrorCode)))
          (flush))))))
    ;(println "ERROR: " (. glu gluErrorString (. gl glGetError)))))

(defn- create-drawable-fn[demo-data-ref]
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

(defn create-particle-demo [frame retval]
  (let [uigl    (ui-get-gl-window-data frame)
        ms-data (dmut-create-multisample-data 
                  frame 
                  #(create-drawable-fn retval))]
    (dosync (ref-set retval ms-data))
    ;    (uigl-disable-fps-animator uigl)
    (dosync (ref-set *particles* (make-particles 100)))
    (uigl-set-fps-animator uigl 60)
    (create-drawable-fn retval)))

(defn destroy-particle-demo
  "Release the resources associated with this demo"
  [demo-data-ref]
  (dmut-delete-multisample-render-data @demo-data-ref))

