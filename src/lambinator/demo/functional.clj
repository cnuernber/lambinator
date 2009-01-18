(ns lambinator.demo.functional
  (:use lambinator.demo.functionalgraphics
	lambinator.demo.fishparts
	lambinator.demo.util
	lambinator.ui
	lambinator.ui.gl
	lambinator.ui.inspector)
  (:import (javax.media.opengl GL DebugGL)
	   (javax.media.opengl.glu GLU)))


(defn create-fishes []
  (*fishes* (float3x3-identity)))

(defn min-max-lines [fishes]
	(reduce (fn [[minx miny maxx maxy] [{sx :x sy :y} {ex :x ey :y}]]
		  [(min minx sx ex) (min miny sy ey)
		   (max maxx sx ex) (max maxy sy ey)])
		[ 0 0 0 0 ] fishes))

(defn render-fishes [ drawable fishes min-maxes ]
  (let [#^GL gl (DebugGL. (.getGL drawable))
	[minx miny maxx maxy] min-maxes
	#^GLU glu (GLU.)]
    (. gl glUseProgram 0 )
    (. gl glClearColor 0.05 0.05 0.1 1.0)
    (. gl glClear GL/GL_COLOR_BUFFER_BIT)
    (. gl glShadeModel GL/GL_SMOOTH)
    (. gl glDisable GL/GL_BLEND)
    (. gl glMatrixMode GL/GL_PROJECTION)
    (. gl glLoadIdentity)
    (. glu gluOrtho2D minx maxx miny maxy)
    (. gl glMatrixMode GL/GL_MODELVIEW)
    (. gl glLoadIdentity)
    (. gl glBegin GL/GL_LINES )
    (doseq [[{sx :x sy :y} {ex :x ey :y}] fishes]
      (. gl glColor4f 0.0 0.5 1.5 1.0)
      (. gl glVertex2f sx sy)
      (. gl glColor4f 0.5 0.5 0.0 1.0)
      (. gl glVertex2f ex ey))
    (. gl glEnd)))

(defn create-and-set-drawable-fn[demo-data-ref fishes]
  (let [num-samples @(@demo-data-ref :num-samples-ref)
	min-maxes (min-max-lines fishes)
	local-drawable #(render-fishes % fishes min-maxes)
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

(defn dmfn-destroy-demo-data
  "Release the resources associated with this demo"
  [demo-data-ref]
  (dmut-delete-multisample-render-data @demo-data-ref))

(defn dmfn-create-demo-data
  "Create the fish demo data, and place in the retval item"
  [frame retval]
  (let [fishes (create-fishes)
	uigl (ui-get-gl-window-data frame)
	ms-data (dmut-create-multisample-data 
		 frame 
		 #(create-and-set-drawable-fn retval fishes))]
    (dosync (ref-set retval ms-data))
;    (uigl-disable-fps-animator uigl)
    (uigl-set-fps-animator uigl 5)
    (create-and-set-drawable-fn retval fishes)))
