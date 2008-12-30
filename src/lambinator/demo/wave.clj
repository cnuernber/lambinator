(ns lambinator.demo.wave
  (:use lambinator.ui lambinator.rcgl lambinator.rc
	clojure.contrib.seq-utils)
  (:import (java.io File)
	  (javax.media.opengl GL DebugGL)
	  (javax.media.opengl.glu GLU)))


(defstruct wave_demo_data :frame :wave_freq :wave_width :wave_height)

(defn create_wave_demo_data_ref[]
  (ref (struct-map wave_demo_data
	 :frame (ui_create_app_frame "wave demo")
	 :wave_freq 1.0
	 :wave_width 0.1
	 :wave_height 3.0)))

(defn with_context_and_tasks_refs[wave_demo_data_ref lambda]
  (let [fm (@wave_demo_data_ref :frame)
	rc_ref (ui_get_rcgl_render_context_ref fm)
	render_tasks_ref (ui_get_render_todo_list_ref fm)]
    (lambda rc_ref render_tasks_ref)))

;given a frame, load the wave glsl* files
(defn load_wave_program [wave_demo_data_ref]
  (with_context_and_tasks_refs 
   wave_demo_data_ref
   #(rcgl_create_glsl_program 
     %1 
     %2
     "../data/glsl/wave.glslv" 
     "../data/glsl/wave.glslf"
     "wave")))

(defn delete_wave_program [wave_demo_data_ref]
  (with_context_and_tasks_refs
   wave_demo_data_ref
   #(rcgl_delete_glsl_program %1 %2 "wave")))


(defn set_wave_frequency [wave_demo_data_ref val] 
  (dosync (ref-set wave_demo_data_ref (assoc @wave_demo_data_ref :wave_freq val))))

(defn set_wave_width [wave_demo_data_ref val] 
  (dosync (ref-set wave_demo_data_ref (assoc @wave_demo_data_ref :wave_height val))))

(defn set_wave_height [wave_demo_data_ref val] 
  (dosync (ref-set wave_demo_data_ref (assoc @wave_demo_data_ref :wave_width val))))

(defn general_display_wave_demo[drawable wave_proggy display_predicate 
				wave_time wave_width wave_height geom_fn]
  (let [#^GL real_gl (. drawable getGL)
	#^GL gl (DebugGL. real_gl)
	#^GLU glu (GLU.)]
    (. gl glClearColor 0.0 0.0 0.0 1.0)
    (. gl glClear GL/GL_COLOR_BUFFER_BIT)
    (when (display_predicate)
      (. gl glUseProgram (wave_proggy :gl_handle))
      (. gl glShadeModel GL/GL_SMOOTH)
      (. gl glPolygonMode GL/GL_FRONT_AND_BACK GL/GL_LINE)
      (. gl glMatrixMode GL/GL_PROJECTION)
      (. gl glLoadIdentity )
      (. glu gluPerspective 40  1 0.0001 1000.0)
      (. gl glMatrixMode GL/GL_MODELVIEW)
      (. gl glLoadIdentity)
      (. gl glTranslatef 0.0 0.0 -150.0)
      (. gl glRotatef -45.0 1.0 0.0 0.0)
      (rcgl_set_glsl_prog_uniforms 
       gl
       [["waveTime" wave_time]
	["waveWidth" wave_width]
	["waveHeight" wave_height]]
       wave_proggy )
      (geom_fn gl)
      (. gl glUseProgram 0))))
  
;runs one display loop of the wave demo.
;for a good time, remove the type inferencing support and see how long it takes...
;You could make this a *lot* faster by switching the vertex shader to use
;a vertex array attribute and calling glDrawArrays.
;Finally, it would be good to get this running on an FBO with full multi-sample
;support as you would see a lot fewer lines.
;Finally, the coolest thing you could do would be to create a normal map
;and manipulate the normal map such that you got better wave looks without
;millions of more polygons.
(defn display_simple_wave_demo [drawable render_context_ref wave_time wave_width wave_height]
  (let [wave_proggy (rcgl_get_glsl_program @render_context_ref "wave")]
    (general_display_wave_demo 
     drawable 
     wave_proggy 
     (fn [] wave_proggy) 
     wave_time
     wave_width
     wave_height
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

(defn display_animating_wave_demo [drawable render_context_ref wave_demo_data_ref start_milliseconds demo_fn]
  (let [current (System/currentTimeMillis)
	wave_frequency (@wave_demo_data_ref :wave_freq)
	relative (- current start_milliseconds)
	relative_seconds (/ relative 1000)
	rel_seq_freq (* relative_seconds wave_frequency)
	wave_time (rem rel_seq_freq Math/PI)
	wave_width (@wave_demo_data_ref :wave_width)
	wave_height (@wave_demo_data_ref :wave_height)]
    (demo_fn drawable render_context_ref wave_time wave_width wave_height)))

(defn setup_wave_demo[wave_demo_data_ref demo_fn]
  (let [fm (@wave_demo_data_ref :frame)
	current_millis (System/currentTimeMillis)
	rc_ref (ui_get_rcgl_render_context_ref fm)]
    (ui_set_gl_render_fn fm #(display_animating_wave_demo % rc_ref wave_demo_data_ref current_millis demo_fn))
    (ui_set_fps_animator fm 60)
    nil))

(defn enable_simple_wave_demo[wave_demo_data_ref]
  (load_wave_program wave_demo_data_ref)
  (setup_wave_demo wave_demo_data_ref display_simple_wave_demo))

;generate a set of nexted vectors, one for each quad.
;flatten this out into straight up vectors
(defn wave_geom_generator[]
  (map 
   float 
   (flatten 
    (for [i (range (float -50) (float 50)) j (range (float -50) (float 50))]
      [i j
       (+ i 1) j
       (+ i 1) (+ j 1)
       i (+ j 1)]))))

(defn create_wave_vbo[wave_demo_data_ref]
  (with_context_and_tasks_refs 
   wave_demo_data_ref
   #(rcgl_create_vbo %1 %2 "wave_data" :data wave_geom_generator)))

(defn delete_wave_vbo[wave_demo_data_ref]
  (with_context_and_tasks_refs 
   wave_demo_data_ref
   #(rcgl_delete_vbo %1 %2 "wave_data" )))


(defn display_vbo_wave_demo [drawable render_context_ref wave_time wave_width wave_height]
  (let [wave_proggy (rcgl_get_glsl_program @render_context_ref "wave")
	wave_data (rcgl_get_vbo @render_context_ref "wave_data")]
    (general_display_wave_demo 
     drawable 
     wave_proggy 
     (fn [] (and wave_proggy wave_data)) 
     wave_time
     wave_width
     wave_height
     (fn [#^GL gl]
       (. gl glEnableClientState GL/GL_VERTEX_ARRAY)
       (. gl glBindBuffer (vbo_gl_type_from_vbo_type (wave_data :type)) (wave_data :gl_handle))
       (. gl glVertexPointer (int 2) (int (wave_data :gl_datatype)) (int 0) (long 0))
       ;glDrawArrays takes the index count, not the polygon count or the array item count
       (. gl glDrawArrays GL/GL_QUADS 0 (/ (wave_data :item_count) 2)) ;each index has an x and y
       (. gl glDisableClientState GL/GL_VERTEX_ARRAY)
       (. gl glBindBuffer (vbo_gl_type_from_vbo_type (wave_data :type)) 0)
       (. gl glUseProgram 0)))))

(defn enable_vbo_wave_demo[wave_demo_data_ref]
  (load_wave_program wave_demo_data_ref)
  (create_wave_vbo wave_demo_data_ref)
  (setup_wave_demo wave_demo_data_ref display_vbo_wave_demo))

(defn disable_wave_demo[wave_demo_data_ref]
  (let [fm (@wave_demo_data_ref :frame)]
    (ui_set_gl_render_fn fm nil)
    (ui_set_fps_animator fm 5) ;just ensure the window refreshes regularly
    (delete_wave_program wave_demo_data_ref)
    (delete_wave_vbo wave_demo_data_ref)
    nil))
    