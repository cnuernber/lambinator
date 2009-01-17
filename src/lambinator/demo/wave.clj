(. (System/getProperties) setProperty 
   "com.apple.mrj.application.apple.menu.about.name" "Wave Demo")
(ns lambinator.demo.wave
  (:use lambinator.ui lambinator.rcgl lambinator.rc
	clojure.contrib.seq-utils lambinator.util
	lambinator.log lambinator.ui.inspector
	lambinator.fs lambinator.ui.gl
	lambinator.rcgl.vbo
	lambinator.rcgl.texture
	lambinator.demo.util )
  (:import (java.io File)
	  (javax.media.opengl GL DebugGL)
	  (javax.media.opengl.glu GLU)
	  (java.util Timer)
	  (javax.swing SwingUtilities))
  (:gen-class))

(defn get-demo-gl-window-data [wave-demo-data-ref]
  (let [fm (@wave-demo-data-ref :frame)
	gl-window-data (ui-get-gl-window-data fm)]
    gl-window-data))

(defn with-context-and-tasks-refs[wave-demo-data-ref lambda]
  (uigl-with-render-context-ref-and-todo-list-ref 
   (get-demo-gl-window-data wave-demo-data-ref)
   lambda))

;given a frame, load the wave glsl* files
(defn load-wave-program [wave-demo-data-ref]
  (with-context-and-tasks-refs 
   wave-demo-data-ref
   #(rcgl-create-glsl-program 
     %1 
     %2
     "/data/glsl/wave.glslv" 
     "/data/glsl/wave.glslf"
     "wave")))

(defn delete-wave-program [wave-demo-data-ref]
  (with-context-and-tasks-refs
   wave-demo-data-ref
   (fn [rc rl]
     (rcgl-delete-glsl-program rc rl "wave")
     (rcgl-delete-glsl-program rc rl "wave-edited"))))


(defn set-wave-frequency [wave-demo-data-ref val] 
  (dosync (ref-set wave-demo-data-ref (assoc @wave-demo-data-ref :wave-freq val))))

(defn set-wave-width [wave-demo-data-ref val] 
  (dosync (ref-set wave-demo-data-ref (assoc @wave-demo-data-ref :wave-height val))))

(defn set-wave-height [wave-demo-data-ref val] 
  (dosync (ref-set wave-demo-data-ref (assoc @wave-demo-data-ref :wave-width val))))

(defn general-display-wave-demo[render-context-ref drawable wave-proggy display-predicate 
				wave-time wave-width wave-height geom-fn]
  (let [#^GL real-gl (. drawable getGL)
	#^GL gl (DebugGL. real-gl)
	#^GLU glu (GLU.)
	logger-ref (@render-context-ref :logger-ref)]
    (. gl glClearColor 0.0 0.0 0.2 1.0)
    (. gl glClear GL/GL_COLOR_BUFFER_BIT)
    (if (display-predicate)
      (do
	(. gl glUseProgram (wave-proggy :gl-handle))
	(. gl glShadeModel GL/GL_SMOOTH)
	(. gl glPolygonMode GL/GL_FRONT_AND_BACK GL/GL_LINE)
	(. gl glMatrixMode GL/GL_PROJECTION)
	(. gl glLoadIdentity )
	(. glu gluPerspective 40  1 0.0001 1000.0)
	(. gl glMatrixMode GL/GL_MODELVIEW)
	(. gl glLoadIdentity)
	(. gl glTranslatef 0.0 0.0 -150.0)
	(. gl glRotatef -45.0 1.0 0.0 0.0)
	(rcgl-set-glsl-uniforms 
	 @render-context-ref
	 gl
	 [["waveTime" wave-time]
	  ["waveWidth" wave-width]
	  ["waveHeight" wave-height]]
	 wave-proggy )
	(geom-fn gl)
	(. gl glUseProgram 0))
      (log-message @logger-ref "wave demo:" :info "Missing resources for render"))))


(defn get-first-valid-glsl-program [render-context program-name-seq]
  (first (filter identity (map #(rcgl-get-glsl-program render-context %) program-name-seq))))
  
;runs one display loop of the wave demo.
;for a good time, remove the type inferencing support and see how long it takes...
;You could make this a *lot* faster by switching the vertex shader to use
;a vertex array attribute and calling glDrawArrays.
;Finally, it would be good to get this running on an FBO with full multi-sample
;support as you would see a lot fewer lines.
;Finally, the coolest thing you could do would be to create a normal map
;and manipulate the normal map such that you got better wave looks without
;millions of more polygons.
(defn display-simple-wave-demo [drawable render-context-ref wave-time wave-width wave-height]
  (let [wave-proggy (get-first-valid-glsl-program @render-context-ref ["wave-edited" "wave"])]
    (general-display-wave-demo 
     render-context-ref
     drawable 
     wave-proggy 
     (fn [] wave-proggy) 
     wave-time
     wave-width
     wave-height
     (fn [#^GL gl]
      ;Draw here a plain surface
       (. gl glBegin GL/GL_QUADS)
       (dorun ;40,000 gl vertex 2f calls.  Not the most elegant way of doing it.
	(for [i (range (float -50) (float 50)) j (range (float -50) (float 50))] 
	  (do
	    (. gl glVertex2f i j)
	    (. gl glVertex2f (+ i 1) j)
	    (. gl glVertex2f (+ i 1) (+ j 1))
	    (. gl glVertex2f i  (+ j 1)))))
       (. gl glEnd )))))

(defn display-animating-wave-demo [drawable render-context-ref wave-demo-data-ref start-milliseconds demo-fn]
  (let [current (System/currentTimeMillis)
	wave-frequency (@wave-demo-data-ref :wave-freq)
	relative (- current start-milliseconds)
	relative-seconds (/ relative 1000)
	rel-seq-freq (* relative-seconds wave-frequency)
	wave-time (rem rel-seq-freq Math/PI)
	wave-width (@wave-demo-data-ref :wave-width)
	wave-height (@wave-demo-data-ref :wave-height)]
    (demo-fn drawable render-context-ref wave-time wave-width wave-height)))

;generate a set of nexted vectors, one for each quad.
;flatten this out into straight up vectors
(defn wave-geom-generator[]
  (map 
   float 
   (flatten 
    (for [i (range (float -50) (float 50)) j (range (float -50) (float 50))]
      [i j
       (+ i 1) j
       (+ i 1) (+ j 1)
       i (+ j 1)]))))

(defn create-wave-vbo[wave-demo-data-ref]
  (with-context-and-tasks-refs 
   wave-demo-data-ref
   #(rcgl-create-vbo %1 %2 "wave-data" :data wave-geom-generator)))

(defn delete-wave-vbo[wave-demo-data-ref]
  (with-context-and-tasks-refs 
   wave-demo-data-ref
   #(rcgl-delete-vbo %1 %2 "wave-data" )))


(defn display-vbo-wave-demo [drawable render-context-ref wave-time wave-width wave-height]
  (let [wave-proggy (get-first-valid-glsl-program @render-context-ref ["wave-edited" "wave"])
	wave-data (rcgl-get-vbo @render-context-ref "wave-data")]
    (general-display-wave-demo 
     render-context-ref
     drawable 
     wave-proggy 
     (fn [] (and wave-proggy wave-data)) 
     wave-time
     wave-width
     wave-height
     (fn [#^GL gl]
       (. gl glEnableClientState GL/GL_VERTEX_ARRAY)
       (. gl glBindBuffer (rcglv-gl-type-from-vbo-type (wave-data :type)) (wave-data :gl-handle))
       (. gl glVertexPointer (int 2) (int (wave-data :gl-datatype)) (int 0) (long 0))
       ;glDrawArrays takes the index count, not the polygon count or the array item count
       (. gl glDrawArrays GL/GL_QUADS 0 (/ (wave-data :item-count) 2)) ;each index has an x and y
       (. gl glDisableClientState GL/GL_VERTEX_ARRAY)
       (. gl glBindBuffer (rcglv-gl-type-from-vbo-type (wave-data :type)) 0)
       (. gl glUseProgram 0)))))

(defmulti get-wave-demo-fn (fn [demo-ref] (@demo-ref :geom-type)))
(defmethod get-wave-demo-fn :default [_] display-vbo-wave-demo)
(defmethod get-wave-demo-fn :immediate [_] display-simple-wave-demo)

(defn create-wave-drawable-fn[wave-demo-data-ref demo-fn]
  (let [fm (@wave-demo-data-ref :frame)
	current-millis (System/currentTimeMillis)
	rc-ref (uigl-get-render-context-ref (ui-get-gl-window-data fm))]
    #(display-animating-wave-demo % rc-ref wave-demo-data-ref current-millis demo-fn)))

(defn disable-wave-demo[wave-demo-data-ref]
  (let [gl-window-data (get-demo-gl-window-data wave-demo-data-ref)]
    (uigl-set-render-fn gl-window-data nil)
    (uigl-set-fps-animator gl-window-data 5) ;just ensure the window refreshes regularly
    (delete-wave-program wave-demo-data-ref)
    (delete-wave-vbo wave-demo-data-ref)
    (dmut-delete-multisample-render-data (@wave-demo-data-ref :multisample-data)))
  nil)

(defonce geom-choices-array [:immediate :vbo])
(defstruct wave-demo-data :frame :wave-freq :wave-width :wave-height 
	   :multisample-data :geom-type :inspector-items
	   :glslv-edited :glslf-edited)

(defn reset-wave-demo[demo-data-ref]
  (load-wave-program demo-data-ref)
  (create-wave-vbo demo-data-ref)
  (let [multisample-data (@demo-data-ref :multisample-data)
	num-samples @(multisample-data :num-samples-ref)
	local-drawable-fn  (create-wave-drawable-fn demo-data-ref (get-wave-demo-fn demo-data-ref))
	drawable-fn (if (= num-samples :none)
		      local-drawable-fn
		      (do 
			(dmut-create-multisample-render-data multisample-data)
			(dmut-create-aa-drawable-fn multisample-data local-drawable-fn)))
	gl-window-data (get-demo-gl-window-data demo-data-ref)]
    (uigl-set-render-fn gl-window-data drawable-fn)
    (uigl-set-fps-animator gl-window-data 60)))

(defn demo-setup-inspector-panel [wave-demo-data-ref]
  (let [demo-data @wave-demo-data-ref
	frame (demo-data :frame)
	items (demo-data :inspector-items)]
    (uii-setup-inspector-panel (frame :inspector-pane) (demo-data :inspector-items))))

(defn get-first-non-null[items]
  (first (filter #(not(nil? %)) items)))

;if the file doesn't exist in the temp directory	
(defn handle-wave-glsl-edit [wave-demo-ref keyword resource-name]
  (when-not (@wave-demo-ref keyword)
    ;perform copy to temp dir
    (let [new-temp-file (fs-get-temp-file resource-name)
	  new-temp-fname (. new-temp-file getCanonicalPath)
	  frame (@wave-demo-ref :frame)
	  render-context-ref ((frame :win-data ) :render-context-ref)
	  watcher-system (frame :file-watcher-system)]
      (dosync (alter wave-demo-ref #(assoc % keyword new-temp-fname))) ;remember that we have been here
      (ui-add-hook frame :close-hooks-ref 
		   (fn []
		     (dosync (alter wave-demo-ref #(assoc % keyword nil))) ;no more associated
		     (. (File. new-temp-fname) delete)))
      ;create new valid edited program
      (let [vert-shader (get-first-non-null [(@wave-demo-ref :glslv-edited) "/data/glsl/wave.glslv"])
	    frag-shader (get-first-non-null [(@wave-demo-ref :glslf-edited) "/data/glsl/wave.glslf"])]
	(fs-copy-item-stream-to-file resource-name new-temp-fname)
	(with-context-and-tasks-refs
	 wave-demo-ref
	 (fn [rc rl]
	   (rcgl-create-glsl-program 
	    rc 
	    rl
	    vert-shader 
	    frag-shader
	    "wave-edited")))
	(fs-add-file-mod-watcher 
	 watcher-system ;system
	 new-temp-fname ;file to watch
	 keyword ;name of the watcher
	 (fn [fname]
	   (ui-add-log-message frame "wave" :info "reloading shader: " fname)
	   (with-context-and-tasks-refs wave-demo-ref 
					(fn [render-context-ref render-list-ref]
					  (rcgl-load-shader 
					   render-context-ref 
					   render-list-ref 
					   new-temp-fname))))))))
  
  (let [edited (@wave-demo-ref keyword)]
    (util-run-cmd ["open" edited])))

;this has to run from the swing event thread.
(defn do-create-wave-demo[frame retval]
  (dosync (ref-set retval (struct-map wave-demo-data
			    :wave-freq 1.0
			    :wave-width 0.1
			    :wave-height 3.0
			    :multisample-data (dmut-create-multisample-data frame #(reset-wave-demo retval))
			    :geom-type :vbo)))
	  
  (let [aa-item ((@retval :multisample-data) :inspector-item)
	geom-item (uii-create-list-inspector-item
		   "Geom Render Mode: " ;name
		   geom-choices-array   ;options 
		   (fn [] (@retval :geom-type)) ;getter
		   (fn [val] (dosync (ref-set retval (assoc @retval :geom-type val)))
		     (reset-wave-demo retval)) ;setter
		   (fn [item] ;stringify
		     (if (= item :immediate)
		       "immediate"
		       "vertex buffer object"
		       )))
	freq-item (uii-create-float-slider-inspector-item
		   "Wave Frequency: "
		   0
		   30
		   (fn [] (@retval :wave-freq))
		   (fn [val] (dosync (ref-set retval (assoc @retval :wave-freq val))))
		   "00.0")
	width-item (uii-create-float-slider-inspector-item
		    "Wave Width: "
		    0
		    1
		   (fn [] (@retval :wave-width))
		   (fn [val] (dosync (ref-set retval (assoc @retval :wave-width val))))
		   "0.00")
	height-item (uii-create-float-slider-inspector-item
		     "Wave Height: "
		     0
		     100
		     (fn [] (@retval :wave-height))
		     (fn [val] (dosync (ref-set retval (assoc @retval :wave-height val))))
		     "000.0")
	glslv-item (uii-create-read-only-hyperlink-inspector-item 
		    "Vertex Shader: "
		    (fn [] "/data/glsl/wave.glslv")
		    #(handle-wave-glsl-edit retval :glslv-edited "/data/glsl/wave.glslv"))
	glslf-item (uii-create-read-only-hyperlink-inspector-item 
		    "Fragment Shader: "
		    (fn [] "/data/glsl/wave.glslf")
		    #(handle-wave-glsl-edit retval :glslf-edited "/data/glsl/wave.glslf"))
	inspector-items [aa-item geom-item freq-item width-item height-item glslv-item glslf-item]]
    (dosync (ref-set retval (assoc @retval :inspector-items inspector-items :frame frame)))
    (demo-setup-inspector-panel retval)
    (reset-wave-demo retval)
    (. (frame :frame) setVisible true)
    retval))


(defn create-wave-demo[]
  (let [retval (ref nil)]
    (SwingUtilities/invokeLater (fn []
				  (let [frame (ui-create-app-frame "Wave Demo" )]
				    (do-create-wave-demo frame retval))))
    retval))

(defn- -main [& args]
  (let [demo-data (create-wave-demo)]
    (SwingUtilities/invokeLater (fn []
				  (disable-wave-demo demo-data)
				  (ui-add-hook (@demo-data :frame) :close-hooks-ref (fn [] (System/exit 0)))))))