(ns lambinator.demo.image
  (:use lambinator.demo.util
	lambinator.ui
	lambinator.ui.gl
	lambinator.ui.inspector
	lambinator.scene
	lambinator.scene-engine
	lambinator.rcgl
	lambinator.rcgl.texture)
  (:import (javax.media.opengl GL DebugGL)
	   (javax.media.opengl.glu GLU)))

(defstruct image-demo-data
  :scene-engine-ref  ;Engine converting between scene and global transform
  :scene-ref         ;Scene
  :multisample-data  ;multisample demo data
  :frame )           ;frame

(defn- render-images [drawable render-context-ref scene-ref scene-engine-ref]
  (let [#^GL gl (DebugGL. (.getGL drawable))
	#^GLU glu (GLU.)
	width (. drawable getWidth)
	height (. drawable getHeight)
	half-width (/ width 2)
	half-height (/ height 2)]
    (try
    ;Take the latest scene and update the scene engine with it.
    ;This work should really happen in another thread perhaps kicked
    ;off by the renderer.
    (when (sne-dirty? @scene-engine-ref @scene-ref)
      (let [engine (sne-update @scene-engine-ref @scene-ref)]
	(dosync (ref-set scene-engine-ref engine))))
    (doto gl
      (.glDisable GL/GL_BLEND)
      (.glClearColor 0.1 0.1 0.1 1.0)
      (.glClear GL/GL_COLOR_BUFFER_BIT)
      (.glShadeModel GL/GL_SMOOTH)
      (.glMatrixMode GL/GL_PROJECTION)
      (.glLoadIdentity))
    (. glu gluOrtho2D (- half-width) half-width (- half-height) half-height)
    (doto gl
      (.glMatrixMode GL/GL_MODELVIEW)
      (.glLoadIdentity))
    (let [engine @scene-engine-ref
	  scene (engine :scene)
	  scene-graph-map (engine :scene-graph-map)
	  global-xforms (engine :global-transforms)
	  render-context @render-context-ref
	  item-nodes (sne-scene-item-list scene scene-graph-map global-xforms)
	  ms-vbo (rcgl-get-vbo render-context dmut-square-vbo-name)
	  image-prog (rcgl-get-glsl-program render-context "basic_image")]
      (when (and ms-vbo image-prog)
	(let [tex-att ((image-prog :attributes) "input_tex_coords")
	      vertex-att ((image-prog :attributes) "input_vertex_coords")]
	(.glEnable gl GL/GL_TEXTURE_2D)
	(.glUseProgram gl (image-prog :gl-handle))
	(dmut-render-setup-square-vbo gl render-context vertex-att tex-att)
	(doseq [[node [global inv-tpos]] item-nodes]
	  (let [item (first (node :items))
		name (item :image)
		render-image (rcgl-get-named-image @render-context-ref name)]
	    (when render-image
	      (let [texture (render-image :gl-image)
		    handle (render-image :gl-handle)
		    tex-width (float (.getWidth texture))
		    tex-height (float (.getHeight texture))]
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
		  ["global_transform" global]
		  ["inverse_tpos" inv-tpos]
		  ["image_pixel_size" [tex-width tex-height]]]
		 image-prog)
		(dmut-render-square-vbo gl)
		(. gl glBindTexture GL/GL_TEXTURE_2D 0)
		(.glDisable gl GL/GL_TEXTURE_2D)
		(.glUseProgram gl 0)
		)))))))
    (catch Exception e (.printStackTrace e)))))
	      
(defn- create-and-set-drawable-fn[demo-data-ref]
  (let [multisample-data (@demo-data-ref :multisample-data)
	num-samples @(multisample-data :num-samples-ref)
	scene-engine-ref (@demo-data-ref :scene-engine-ref)
	frame (@demo-data-ref :frame)
	scene-ref (@demo-data-ref :scene-ref)
	uigl (ui-get-gl-window-data frame)
	rcgl-ref (uigl-get-render-context-ref uigl)
	local-drawable #(render-images % rcgl-ref scene-ref scene-engine-ref)
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
	uigl (ui-get-gl-window-data frame)]
    (uigl-with-render-context-ref-and-todo-list-ref 
     uigl
     (fn [rc rl]
       (rcgl-destroy-named-image rc rl "blue_angle_swirl")
       (dmut-destroy-square-vbo rc rl)))
    (dmut-delete-multisample-render-data multisample-data)))

(defn dmim-create-demo-data
  "Create the image demo data
frame - the ui frame
retval - empty ref to hold return value"
  [frame retval]
  (let [uigl (ui-get-gl-window-data frame)
	ms-data (dmut-create-multisample-data
		 frame
		 #(create-and-set-drawable-fn retval))
	demo-data (struct-map image-demo-data
		    :scene-engine-ref (ref sne-empty-engine)
		    :scene-ref (ref sn-empty-scene)
		    :multisample-data ms-data
		    :frame frame)
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
       (dmut-create-square-vbo rc rl)))
    
    ;(uigl-set-fps-animator uigl 5)
    (create-and-set-drawable-fn retval)))