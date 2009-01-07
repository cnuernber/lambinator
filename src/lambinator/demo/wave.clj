(. (System/getProperties) setProperty 
   "com.apple.mrj.application.apple.menu.about.name" "Wave Demo")
(ns lambinator.demo.wave
  (:use lambinator.ui lambinator.rcgl lambinator.rc
	clojure.contrib.seq-utils lambinator.util
	lambinator.log lambinator.ui.inspector
	lambinator.fs)
  (:import (java.io File)
	  (javax.media.opengl GL DebugGL)
	  (javax.media.opengl.glu GLU)
	  (java.util Timer)
	  (javax.swing SwingUtilities))
  (:gen-class))

(defn with-context-and-tasks-refs[wave-demo-data-ref lambda]
  (let [fm (@wave-demo-data-ref :frame)
	rc-ref (ui-get-rcgl-render-context-ref fm)
	render-tasks-ref (ui-get-render-todo-list-ref fm)]
    (lambda rc-ref render-tasks-ref)))

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

(defn create-wave-drawable-fn[wave-demo-data-ref demo-fn]
  (let [fm (@wave-demo-data-ref :frame)
	current-millis (System/currentTimeMillis)
	rc-ref (ui-get-rcgl-render-context-ref fm)]
    #(display-animating-wave-demo % rc-ref wave-demo-data-ref current-millis demo-fn)))
  

(defn setup-wave-demo[wave-demo-data-ref demo-fn]
  (let [fm (@wave-demo-data-ref :frame)]
    (ui-set-gl-render-fn fm (create-wave-drawable-fn wave-demo-data-ref demo-fn))
    (ui-set-fps-animator fm 60)
    nil))

(defn enable-simple-wave-demo[wave-demo-data-ref]
  (load-wave-program wave-demo-data-ref)
  (setup-wave-demo wave-demo-data-ref display-simple-wave-demo))

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
       (. gl glBindBuffer (vbo-gl-type-from-vbo-type (wave-data :type)) (wave-data :gl-handle))
       (. gl glVertexPointer (int 2) (int (wave-data :gl-datatype)) (int 0) (long 0))
       ;glDrawArrays takes the index count, not the polygon count or the array item count
       (. gl glDrawArrays GL/GL_QUADS 0 (/ (wave-data :item-count) 2)) ;each index has an x and y
       (. gl glDisableClientState GL/GL_VERTEX_ARRAY)
       (. gl glBindBuffer (vbo-gl-type-from-vbo-type (wave-data :type)) 0)
       (. gl glUseProgram 0)))))

(defn enable-vbo-wave-demo[wave-demo-data-ref]
  (load-wave-program wave-demo-data-ref)
  (create-wave-vbo wave-demo-data-ref)
  (setup-wave-demo wave-demo-data-ref display-vbo-wave-demo))

(defn generate-multisample-vbo[]
  (map 
   float
   [-1 -1  0 0
    -1  1  0 1
     1  1  1 1
     1 -1  1 0]))

(defonce aa-choices-array [:none :2 :4 :8 :16])

(defn create-multisample-fbos[wave-demo-data-ref]
  (let [{ { { drawable :gl-win } :win-data } :frame } @wave-demo-data-ref
	width (. drawable getWidth)
	height (. drawable getHeight)
	num-samples (@wave-demo-data-ref :num-samples)
	indexed-choices-array (map vector (iterate inc 0) (rest aa-choices-array))
	[this-choice-index -] (first (filter (fn [[index choice]] (= choice num-samples)) indexed-choices-array))
	;pick aa modes that are less than or equal to the chosen aa mode so we have valid fallbacks
	valid-choices (reverse (filter (fn [[index choice]] (>= this-choice-index index)) indexed-choices-array))
	ms-spec-seq (map (fn [[index choice]]
			   (create-surface-spec 
			    { :color0 (create-renderbuffer :color :ubyte :rgb false) }
			    width
			    height
			    choice))
			 valid-choices )
	trans-spec (create-surface-spec
		    { :color0 (create-renderbuffer :color :ubyte :rgb  true) }
		    width
		    height)]
    (with-context-and-tasks-refs 
     wave-demo-data-ref
     (fn [rc rl]
       (rcgl-create-context-surface-seq rc rl ms-spec-seq "wave-multisample-surface")
       (rcgl-create-context-surface rc rl trans-spec "wave-transfer-surface")))))

(defn create-multisample-data[wave-demo-data-ref]
  (create-multisample-fbos wave-demo-data-ref)
  (with-context-and-tasks-refs 
   wave-demo-data-ref
   (fn [rc rl]
     (rcgl-create-glsl-program 
      rc 
      rl
      "/data/glsl/passthrough.glslv" 
      "/data/glsl/single_texture.glslf"
      "wave-final-render-prog")
     (rcgl-create-vbo rc rl "wave-multisample-vbo" :data #(generate-multisample-vbo)))))

(defn delete-multisample-data[wave-demo-data-ref]
  (with-context-and-tasks-refs 
   wave-demo-data-ref
   (fn [rc rl]
     (rcgl-delete-context-surface rc rl "wave-multisample-surface")
     (rcgl-delete-context-surface rc rl "wave-transfer-surface")
     (rcgl-delete-glsl-program rc rl "wave-final-render-prog")
     (rcgl-delete-vbo rc rl "wave-multisample-vbo"))))

(defn antialiasing-drawable-wrapper[drawable render-context-ref frame-resize-data wave-demo-data-ref child-drawable]
  (let [real-gl (. drawable getGL)
	#^GL gl (DebugGL. real-gl)
	width (. drawable getWidth)
	height (. drawable getHeight)
	render-context @render-context-ref
	ms-surface (rcgl-get-context-surface render-context "wave-multisample-surface")
	transfer-surface (rcgl-get-context-surface render-context "wave-transfer-surface")
	final-prog (rcgl-get-glsl-program render-context "wave-final-render-prog")
	ms-vbo (rcgl-get-vbo render-context "wave-multisample-vbo")
	do-aa-render (and ms-surface transfer-surface final-prog child-drawable ms-vbo)]
    (if do-aa-render
      (let [ms-fbo (ms-surface :gl-handle)
	    transfer-fbo (transfer-surface :gl-handle)
	    prog-handle (final-prog :gl-handle)
	    transfer-tex (((transfer-surface :attachments) :color0) :texture-gl-handle)
	    tex-att-index (((final-prog :attributes) "input_tex_coords") :index)
	    [render-width render-height] ((ms-surface :surface-spec) :size)
	    vbo-dtype (int (ms-vbo :gl-datatype))]
	;have the child render to the multisample fbo
	;the bind function sets where gl will render to
	(. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT ms-fbo)
	(try
	 (. gl glViewport 0 0 render-width render-height)
	 (child-drawable drawable)
					;bind the multisample framebuffer as the read framebuffer source
	 (. gl glBindFramebufferEXT GL/GL_READ_FRAMEBUFFER_EXT ms-fbo)
					;bind the transfer as the draw framebuffer dest
	 (. gl glBindFramebufferEXT GL/GL_DRAW_FRAMEBUFFER_EXT transfer-fbo);
        ;downsample the multisample fbo to the draw framebuffer's texture
	 (. gl glBlitFramebufferEXT 
	    0 0 render-width render-height ;source rect
	    0 0 render-width render-height ;dest rect
	    GL/GL_COLOR_BUFFER_BIT ;what to copy over (just color in our case)
	    GL/GL_NEAREST ) ;how to interpolate intermediate results (there aren't any; the sizes match)

        ;Bind the window's render surface as the target render surface
	 (. gl GL/glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT 0)
	 (. gl glViewport 0 0 width height)
	;Now we render our fullscreen quad
	 (. gl glShadeModel GL/GL_SMOOTH)
	 (. gl glPolygonMode GL/GL_FRONT_AND_BACK GL/GL_FILL)
	 (. gl glMatrixMode GL/GL_PROJECTION)
	 (. gl glLoadIdentity )
	 (. gl glMatrixMode GL/GL_MODELVIEW)
	 (. gl glLoadIdentity)
	 (. gl glUseProgram prog-handle)
	 (. gl glEnableClientState GL/GL_VERTEX_ARRAY)
	 (. gl glEnableVertexAttribArray tex-att-index)
	 (. gl glBindBuffer (vbo-gl-type-from-vbo-type (ms-vbo :type)) (ms-vbo :gl-handle))
	 (. gl glEnable GL/GL_TEXTURE_2D)
	 (. gl glActiveTexture GL/GL_TEXTURE0)
	 (. gl glBindTexture GL/GL_TEXTURE_2D transfer-tex)
	 (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_MIN_FILTER GL/GL_LINEAR)
	 (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_MAG_FILTER GL/GL_LINEAR)
	 (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_WRAP_S GL/GL_CLAMP_TO_EDGE)
	 (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_WRAP_T GL/GL_CLAMP_TO_EDGE)
        ;we have how bound the second set of texture coordinates to tex coord 0
	;each tex coord takes two entries, they have a stride of 4
	;and they are offset from the beginning of the array by two
	 (. gl glVertexAttribPointer 
	    tex-att-index ;index
	    (int 2)       ;size
	    vbo-dtype     ;type
	    false         ;normalized
	    (int 16)       ;stride
	    (long 8))      ;offset
	 (rcgl-set-glsl-uniforms
	  @render-context-ref
	  gl
	  [["tex" 0]] ;set the texture param to desired logical texture unit
	  final-prog )
        ; Render Fullscreen Quad
	 (. gl glVertexPointer (int 2) (int (ms-vbo :gl-datatype)) (int 16) (long 0))
					;glDrawArrays takes the index count, not the polygon count or the array item count
	 (. gl glDrawArrays GL/GL_QUADS 0 (/ (ms-vbo :item-count) 4)) ;each index has an x and y, u and v
	 (. gl glDisableClientState GL/GL_VERTEX_ARRAY)
	 (. gl glActiveTexture GL/GL_TEXTURE0)
	 (. gl glBindBuffer (vbo-gl-type-from-vbo-type (ms-vbo :type)) 0)
	 (finally
	  (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT 0)))) ;make goddamn sure we don't end up with an invalid fbo bound.
      
      (when child-drawable ;if we can't render antialiased because we don't have buffers
	(child-drawable drawable)))
    ;update frame resize data so we know how many times the drawable has rendered at this exact size
    ;we only resize when a certain number of frames have been rendered at a certain size.
    ;this is because resizing fbos is relative expensive and can apparently lead
    ;to fragmentation of video ram (although I doubt the second claim)
    (let [resize-frame-count (@frame-resize-data :resize-frame-count)
	  [rs-width rs-height] (@frame-resize-data :resize-frame-size)
	  resize-frame-count (if (and (== rs-width width)
				      (== rs-height height))
			       (inc resize-frame-count)
			       0)
	  fbos-missing (or (not ms-surface)
			   (not transfer-surface))
	  fbos-size-mismatch (or fbos-missing
				 (not (= ((ms-surface :surface-spec) :size)
					 [width height])))
	  do-resize-fbo (or fbos-missing
			    (and fbos-size-mismatch
				 (> resize-frame-count 10)))]
      (dosync (ref-set frame-resize-data (assoc @frame-resize-data 
					   :resize-frame-count resize-frame-count
					   :resize-frame-size [width height])))
      (when do-resize-fbo
	(create-multisample-fbos wave-demo-data-ref)))
    (when ms-surface
      (let [actual-sample-count ((ms-surface :surface-spec) :multi-sample)]
	(when (not (= (@wave-demo-data-ref :num-samples)
		      actual-sample-count))
	  (dosync (ref-set wave-demo-data-ref (assoc @wave-demo-data-ref :num-samples actual-sample-count)))
	  ;update ui to reflect reality which, due to differenes
	  ;in hardware, may not match what the user wanted.
	  (let [item (first (filter 
			     (fn [item] (= (item :name) "Antialiasing: "))
			     (@wave-demo-data-ref :inspector-items)))]
	    (when item
	      ((item :updater)))))))))

(defmulti get-wave-demo-fn (fn [demo-ref] (@demo-ref :geom-type)))
(defmethod get-wave-demo-fn :default [-] display-vbo-wave-demo)
(defmethod get-wave-demo-fn :immediate [-] display-simple-wave-demo)

(defn create-aa-drawable-fn[wave-demo-data-ref]
  (let [frame-resize-data (ref {:resize-frame-count 0 :resize-frame-size [0 0]})
	fm (@wave-demo-data-ref :frame)
	rc-ref ((fm :win-data) :render-context-ref)
	drawable-fn (create-wave-drawable-fn wave-demo-data-ref (get-wave-demo-fn wave-demo-data-ref))
	aa-drawable-fn #(antialiasing-drawable-wrapper % rc-ref frame-resize-data wave-demo-data-ref drawable-fn)]
    aa-drawable-fn))
  

(defn enable-antialiased-wave-demo [wave-demo-data-ref]
  (let [drawable-fn (create-aa-drawable-fn wave-demo-data-ref)
	fm (@wave-demo-data-ref :frame)]
    (load-wave-program wave-demo-data-ref)
    (create-wave-vbo wave-demo-data-ref)
    (create-multisample-data wave-demo-data-ref)
    (ui-set-gl-render-fn fm drawable-fn)
    (ui-set-fps-animator fm 60)))

(defn disable-wave-demo[wave-demo-data-ref]
  (let [fm (@wave-demo-data-ref :frame)]
    (ui-set-gl-render-fn fm nil)
    (ui-set-fps-animator fm 5) ;just ensure the window refreshes regularly
    (delete-wave-program wave-demo-data-ref)
    (delete-wave-vbo wave-demo-data-ref)
    (delete-multisample-data wave-demo-data-ref)
    nil))

(defonce geom-choices-array [:immediate :vbo])
(defstruct wave-demo-data :frame :wave-freq :wave-width :wave-height 
	   :num-samples :geom-type :inspector-items
	   :glslv-edited :glslf-edited)

(defn reset-wave-demo[demo-data-ref]
  (load-wave-program demo-data-ref)
  (create-wave-vbo demo-data-ref)
  (let [drawable-fn (if (= (@demo-data-ref :num-samples) :none)
		      (create-wave-drawable-fn demo-data-ref (get-wave-demo-fn demo-data-ref))
		      (do
			(create-multisample-data demo-data-ref)
			(create-aa-drawable-fn demo-data-ref)))
	fm (@demo-data-ref :frame)]
    (ui-set-gl-render-fn fm drawable-fn)
    (ui-set-fps-animator fm 60)))

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
(defn do-create-wave-demo[retval]
  (dosync (ref-set retval (struct-map wave-demo-data
			    :wave-freq 1.0
			    :wave-width 0.1
			    :wave-height 3.0
			    :num-samples :4
			    :geom-type :vbo)))
  (let [frame (ui-create-app-frame "Wave Demo")
	aa-item (uii-create-list-inspector-item 
		 "Antialiasing: " ;item name
		 aa-choices-array ;choices
		 (fn [] (@retval :num-samples)) ;getter
		 (fn [val] 
		   (dosync (ref-set retval (assoc @retval :num-samples val)))
		   (reset-wave-demo retval)) ;setter
		 (fn [item]
		   (if (= item :none)
		     "none"
		     (util-stringify (name item) "x"))))
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
    (ui-add-hook frame :close-hooks-ref #(disable-wave-demo retval))
    (demo-setup-inspector-panel retval)
    (reset-wave-demo retval)
    (. (frame :frame) setVisible true)
    retval))


(defn create-wave-demo[]
  (let [retval (ref nil)]
    (SwingUtilities/invokeLater #(do-create-wave-demo retval))
    retval))

(defn- -main [& args]
  (let [demo-data (create-wave-demo)]
    (SwingUtilities/invokeLater #(ui-add-hook (@demo-data :frame) :close-hooks-ref (fn [] (System/exit 0))))))