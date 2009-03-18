(ns lambinator.demo.image
  (:use lambinator.demo.util
	lambinator.ui
	lambinator.ui.gl
	lambinator.ui.inspector
	lambinator.scene
	lambinator.scene-engine)
  (:import (javax.media.opengl GL DebugGL)
	   (javax.media.opengl.glu GLU)))

(defstruct image-demo-data
  :scene-engine-ref  ;Engine converting between scene and global transform
  :scene-ref         ;Scene
  :multisample-data  ;multisample demo data
  :frame )           ;frame

(defn- render-images [drawable scene-ref scene-engine-ref]
  (let [#^GL gl (DebugGL. (.getGL drawable))
	#^GLU glu (GLU.)]
    ;Take the latest scene and update the scene engine with it.
    ;This work should really happen in another thread perhaps kicked
    ;off by the renderer.
    (when (sne-dirty? @scene-engine-ref @scene-ref)
      (let [engine (sne-update @scene-engine-ref @scene-ref)]
	(dosync (ref-set scene-engine-ref engine))))
    (doto gl
      (.glClearColor 0.2 0.2 0.2 1.0)
      (.glClear GL/GL_COLOR_BUFFER_BIT)
      (.glShadeModel GL/GL_SMOOTH)
      (.glMatrixMode GL/GL_PROJECTION)
      (.glLoadIdentity))))

(defn- create-and-set-drawable-fn[demo-data-ref]
  (let [multisample-data (@demo-data-ref :multisample-data)
	num-samples @(multisample-data :num-samples-ref)
	scene-engine-ref (@demo-data-ref :scene-engine-ref)
	frame (@demo-data-ref :frame)
	scene-ref (@demo-data-ref :scene-ref)
	local-drawable #(render-images % scene-ref scene-engine-ref)
	local-drawable (if (= num-samples :none)
			 local-drawable
			 (do
			   (dmut-create-multisample-render-data multisample-data)
			   (dmut-create-aa-drawable-fn multisample-data local-drawable)))
	uigl (ui-get-gl-window-data frame)]
    ;(println @demo-data-ref)
    (uigl-set-render-fn uigl local-drawable)
    (uigl-repaint uigl)))

(defn dmim-destroy-demo-data
  "Release the demo data"
  [demo-data-ref]
  (let [multisample-data (@demo-data-ref :multisample-data)]
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
		    :frame frame)]
    (dosync (ref-set retval demo-data))
    (uii-setup-inspector-panel (frame :inspector-pane) [(ms-data :inspector-item)])
    ;(uigl-set-fps-animator uigl 5)
    (create-and-set-drawable-fn retval)))