(ns lambinator.demo.image
  (:use lambinator.demo.util
	lambinator.ui
	lambinator.ui.gl
	lambinator.ui.inspector
	lambinator.ui.util
	lambinator.scene
	lambinator.scene-engine
	lambinator.rcgl
	lambinator.rcgl.texture
	lambinator.util)
  (:import (javax.media.opengl GL DebugGL)
	   (javax.media.opengl.glu GLU)
	   (com.sun.opengl.util BufferUtil)
	   (java.nio ByteBuffer IntBuffer)))

(defstruct image-demo-data
  :scene-engine-ref  ;Engine converting between scene and global transform
  :scene-ref         ;Scene
  :multisample-data  ;multisample demo data
  :mouse-pos-ref     ;Mouse position
  :render-list-ref   ;last items rendered
  :mouse-listener    ;mouse listener
  :frame             ;frame
  :selection-buffer) ;int buffer used for selection

;mouse information
(defstruct mouse-info
  :posx
  :posy
  :bdown)

(defn- render-basic-image
  "Render a basic image.  You need to have setup the dmut-square-vbo."
  [#^GL gl render-context img-name global-xform inv-tpos img-prog-name]
  (let [ms-vbo (rcgl-get-vbo render-context dmut-square-vbo-name)
	image-prog (rcgl-get-glsl-program render-context img-prog-name)
	render-image (rcgl-get-named-image render-context img-name)]
    (when (and ms-vbo image-prog render-image)
      (let [tex-att ((image-prog :attributes) "input_tex_coords")
	    vertex-att ((image-prog :attributes) "input_vertex_coords")
	    texture (render-image :gl-image)
	    handle (render-image :gl-handle)
	    tex-width (float (.getWidth texture))
	    tex-height (float (.getHeight texture))]
	(.glEnable gl GL/GL_TEXTURE_2D)
	(.glUseProgram gl (image-prog :gl-handle))
	(dmut-render-setup-square-vbo gl render-context vertex-att tex-att)
	(rcglt-tex2d-param gl GL/GL_TEXTURE_MIN_FILTER GL/GL_LINEAR)
	(rcglt-tex2d-param gl GL/GL_TEXTURE_MAG_FILTER GL/GL_LINEAR)
	(rcglt-tex2d-param gl GL/GL_TEXTURE_WRAP_S GL/GL_CLAMP_TO_EDGE)
	(rcglt-tex2d-param gl GL/GL_TEXTURE_WRAP_T GL/GL_CLAMP_TO_EDGE)
	(.glActiveTexture gl GL/GL_TEXTURE0)
	(.glBindTexture gl GL/GL_TEXTURE_2D handle)
	(rcgl-set-glsl-uniforms 
	 render-context
	 gl
	 [["tex" 0]
	  ["global_transform" global-xform]
	  ["inverse_tpos" inv-tpos]
	  ["image_pixel_size" [tex-width tex-height]]]
	 image-prog)
	(dmut-render-square-vbo gl)
	(.glBindTexture gl GL/GL_TEXTURE_2D 0)
	(.glDisable gl GL/GL_TEXTURE_2D)
	(.glUseProgram gl 0)))))

(defn- setup-ortho-projection
  [#^GLU glu width height]
  (let [half-width (/ width 2)
	half-height (/ height 2)]
    (. glu gluOrtho2D (- half-width) half-width (- half-height) half-height)))

(defn- process-hits
  "not safe to call if hits <= 0"
  [hits int-buffer]
  (loop [min-id 0
	 min-dist -1
	 index 0
	 hits (dec hits)]
    (let [id-count (.get int-buffer index)
	  buf-min-dist (.get int-buffer (+ index 1))
	  buf-max-dist (.get int-buffer (+ index 2))
	  first-id (.get int-buffer (+ index 3))
	  index (+ index 3 id-count)
	  [min-id min-dist] (if (util-unsigned-less-equal buf-min-dist min-dist)
			      [first-id buf-min-dist]
			      [min-id min-dist])]
      (if (<= hits 0)
	min-id
	(recur min-id min-dist index hits)))))
  

(defn- calculate-hit-item
  [#^GL gl #^GLU glu render-context width height mouse-pos render-list int-buffer]
  (.clear int-buffer)
  ;reset int buffer
  (try
   (doto gl
     (.glSelectBuffer 1024 int-buffer)
     (.glRenderMode GL/GL_SELECT)
     ;(.glClear GL/GL_COLOR_BUFFER_BIT)
     (.glMatrixMode GL/GL_MODELVIEW)
     (.glLoadIdentity)
     (.glMatrixMode GL/GL_PROJECTION)
     (.glLoadIdentity))
   (.gluPickMatrix glu 
		   (mouse-pos :posx) (- height (mouse-pos :posy)) 
		   5 5 
		   (int-array [0 0 width height]) 0)
   (setup-ortho-projection glu width height)
   (.glInitNames gl)
   (doseq [[node [global inv-tpos]] render-list]
     (let [item (first (node :items))
	   img-name (item :image)]
       (.glPushName gl (node :id))
       (render-basic-image gl render-context img-name global inv-tpos "basic_image")
       (.glPopName gl)))
   (.glFlush gl)
   (let [hits (.glRenderMode gl GL/GL_RENDER)]
     (if (> hits 0)
       (process-hits hits int-buffer)
       0 ))
   (catch Exception e
     (.glRenderMode gl GL/GL_RENDER)
     (throw e))))

(defn- do-render-images
  [#^GL gl #^GLU glu render-context width height engine over-item render-list-ref]
  (doto gl
    (.glDisable GL/GL_BLEND)
    (.glClearColor 0.1 0.1 0.1 1.0)
    (.glClear GL/GL_COLOR_BUFFER_BIT)
    (.glShadeModel GL/GL_SMOOTH)
    (.glMatrixMode GL/GL_PROJECTION)
    (.glLoadIdentity))
  (setup-ortho-projection glu width height)
  (doto gl
    (.glMatrixMode GL/GL_MODELVIEW)
    (.glLoadIdentity))
  (let [scene (engine :scene)
	scene-graph-map (engine :scene-graph-map)
	global-xforms (engine :global-transforms)
	item-nodes (sne-scene-item-list scene scene-graph-map global-xforms)]
    (doseq [[node [global inv-tpos]] item-nodes]
      (let [item (first (node :items))
	    img-name (item :image)
	    img-prog-name (if (== over-item (node :id))
			    "basic_invert"
			    "basic_image")]
	(render-basic-image gl render-context img-name global inv-tpos img-prog-name)))
    (dosync (ref-set render-list-ref item-nodes)))) ;record what was drawn for picking later

(defn- render-images [drawable render-context-ref scene-ref scene-engine-ref 
		      render-list-ref mouse-pos-ref int-buffer]
  (let [#^GL gl (DebugGL. (.getGL drawable))
	#^GLU glu (GLU.)
	width (. drawable getWidth)
	height (. drawable getHeight)
	half-width (/ width 2)
	half-height (/ height 2)
	engine @scene-engine-ref
	scene @scene-ref
	render-list @render-list-ref
	mouse-pos @mouse-pos-ref
	render-context @render-context-ref]
    (try
    ;Take the latest scene and update the scene engine with it.
    ;This work should really happen in another thread perhaps kicked
    ;off by the renderer.
    (when (sne-dirty? engine scene)
      (let [engine (sne-update engine scene)]
	(dosync (ref-set scene-engine-ref engine))))
    (if (and mouse-pos render-list)
      (let [hit-item (calculate-hit-item gl glu render-context width height 
					 mouse-pos render-list int-buffer)]
	(do-render-images gl glu render-context width height
			  @scene-engine-ref hit-item render-list-ref))
      (do-render-images gl glu render-context width height
			@scene-engine-ref 0 render-list-ref))
    (catch Exception e (.printStackTrace e)))))
	      
(defn- create-and-set-drawable-fn[demo-data-ref]
  (let [multisample-data (@demo-data-ref :multisample-data)
	num-samples @(multisample-data :num-samples-ref)
	scene-engine-ref (@demo-data-ref :scene-engine-ref)
	mouse-pos-ref (@demo-data-ref :mouse-pos-ref)
	render-list-ref (@demo-data-ref :render-list-ref)
	int-buffer (@demo-data-ref :int-buffer)
	frame (@demo-data-ref :frame)
	scene-ref (@demo-data-ref :scene-ref)
	uigl (ui-get-gl-window-data frame)
	rcgl-ref (uigl-get-render-context-ref uigl)
	local-drawable #(render-images % rcgl-ref scene-ref scene-engine-ref 
				       render-list-ref mouse-pos-ref int-buffer)
	local-drawable (if (= num-samples :none)
			 local-drawable
			 (do
			   (dmut-create-multisample-render-data multisample-data)
			   (dmut-create-aa-drawable-fn multisample-data local-drawable)))]
    ;(println @demo-data-ref)
    (uigl-set-render-fn uigl local-drawable)
    (uigl-repaint uigl)))

(defn dmim-destroy-demo-data
  "Release the demo data"
  [demo-data-ref]
  (let [multisample-data (@demo-data-ref :multisample-data)
	frame (@demo-data-ref :frame)
	uigl (ui-get-gl-window-data frame)
	mouse-listener (@demo-data-ref :mouse-listener)]
    (uigl-with-render-context-ref-and-todo-list-ref 
     uigl
     (fn [rc rl]
       (rcgl-destroy-named-image rc rl "blue_angle_swirl")
       (dmut-destroy-square-vbo rc rl)))
    (uigl-disable-todo-watcher uigl)
    (uigl-remove-mouse-input-listener uigl mouse-listener)
    (dmut-delete-multisample-render-data multisample-data)))

(defn- create-mouse-listener
  [mouse-info-ref]
  (uiut-create-mouse-input-listener
   {:enter (fn [evt]
	     (dosync (ref-set mouse-info-ref (struct-map mouse-info
					       :posx (.getX evt)
					       :posy (.getY evt)
					       :bdown false))))
    :leave (fn [evt]
	     (dosync (ref-set mouse-info-ref nil)))
    :move (fn [evt]
	    (dosync 
	     (commute mouse-info-ref assoc :posx (.getX evt) :posy (.getY evt))))
    :drag (fn [evt]
	    (dosync 
	     (commute mouse-info-ref assoc :posx (.getX evt) :posy (.getY evt))))
    }))

(defn dmim-create-demo-data
  "Create the image demo data
frame - the ui frame
retval - empty ref to hold return value"
  [frame retval]
  (let [uigl (ui-get-gl-window-data frame)
	mouse-pos-ref (ref nil)
	mouse-listener (create-mouse-listener mouse-pos-ref)
	ms-data (dmut-create-multisample-data
		 frame
		 #(create-and-set-drawable-fn retval))
	int-buffer (BufferUtil/newIntBuffer 1024)
	demo-data (struct-map image-demo-data
		    :scene-engine-ref (ref sne-empty-engine)
		    :scene-ref (ref sn-empty-scene)
		    :multisample-data ms-data
		    :render-list-ref (ref nil)
		    :mouse-pos-ref mouse-pos-ref
		    :mouse-listener mouse-listener
		    :frame frame
		    :int-buffer int-buffer)
	scene-ref (demo-data :scene-ref)
	root-node-id (@scene-ref :root-node)
	[scene img-node] (sn-create-node @scene-ref)
	scene (sn-insert-child scene root-node-id img-node nil)
	item (sn-create-image-item scene "blue_angle_swirl" )
	scene (sn-insert-item scene img-node item 0)]
    (dosync (ref-set retval demo-data)
	    (ref-set scene-ref scene))
    (uii-setup-inspector-panel (frame :inspector-pane) [(ms-data :inspector-item)])
    (uigl-with-render-context-ref-and-todo-list-ref 
     uigl
     (fn [rc rl]
       (rcgl-load-image-file rc rl "/data/images/blue_angle_swirl.jpg" "blue_angle_swirl")
       (rcgl-create-glsl-program rc 
				 rl 
				 "/data/glsl/basic_image.glslv" 
				 "/data/glsl/basic_image.glslf" 
				 "basic_image" )
       (rcgl-create-glsl-program rc 
				 rl 
				 "/data/glsl/basic_image.glslv" 
				 "/data/glsl/basic_invert.glslf" 
				 "basic_invert" )
       (dmut-create-square-vbo rc rl)))
    (uigl-add-mouse-input-listener uigl mouse-listener)
    (uigl-set-fps-animator uigl 30)
    ;(uigl-enable-todo-watcher uigl)
    (create-and-set-drawable-fn retval)
    ))