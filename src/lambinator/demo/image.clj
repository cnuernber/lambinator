(ns lambinator.demo.image
  (:use lambinator.demo.util
	lambinator.ui
	lambinator.ui.gl
	lambinator.ui.inspector
	lambinator.scene
	lambinator.scene-engine
	lambinator.rcgl)
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
	  item-nodes (sne-scene-item-list scene scene-graph-map global-xforms)]
      (doseq [[node [global inv-tpos]] item-nodes]
	(let [item (first (node :items))
	      name (item :image)
	      render-image (rcgl-get-named-image @render-context-ref name)]
	  (when render-image
	    (let [texture (render-image :gl-image)
		  handle (render-image :gl-handle)
		  tex-width (float (.getWidth texture))
		  tex-height (float (.getHeight texture))
		  half-width (/ tex-width 2.0)
		  half-height (/ tex-height 2.0)]
	      (doto gl
		(.glBegin GL/GL_QUADS)
		(.glColor3f 1.0 0.0 0.0)
		(.glVertex2f (- half-width) (- half-height) )
		(.glColor3f 0.0 1.0 0.0)
		(.glVertex2f half-width (- half-height))
		(.glColor3f 0.0 0.0 1.0)
		(.glVertex2f half-width half-height)
		(.glVertex2f (- half-width) half-height)
		(.glEnd))
	      )))))
    (catch Exception e (.printStackTrace e)))
    ))
	      
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
     #(rcgl-destroy-named-image %1 %2 "blue_angle_swirl"))
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
     #(rcgl-load-image-file %1 %2 "/data/images/blue_angle_swirl.jpg" "blue_angle_swirl"))
    
    ;(uigl-set-fps-animator uigl 5)
    (create-and-set-drawable-fn retval)))