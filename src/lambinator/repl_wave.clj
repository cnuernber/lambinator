(use 'lambinator.ui)
(use 'lambinator.rcgl)
(import '(java.io File))
(import '(javax.media.opengl GL DebugGL))
(import '(javax.media.opengl.glu GLU))

;given a frame, load the wave glsl* files
(defn load_wave_program [fm]
  (let [rc_ref (ui_get_rcgl_render_context_ref fm)
	render_tasks_ref (ui_get_render_todo_lists_ref fm)]
    (rcgl_create_glsl_program 
     rc_ref 
     render_tasks_ref 
     "../data/glsl/wave.glslv" 
     "../data/glsl/wave.glslf"
     "wave")))

(defn delete_wave_program [fm]
  (let [rc_ref (ui_get_rcgl_render_context_ref fm)
	render_tasks_ref (ui_get_render_todo_lists_ref fm)]
    (rcgl_delete_glsl_program rc_ref render_tasks_ref "wave")))
  

;runs one display loop of the wave demo.
;for a good time, remove the type inferencing support and see how long it takes...
;You could make this a *lot* faster by switching the vertex shader to use
;a vertex array attribute and calling glDrawArrays.
;Finally, it would be good to get this running on an FBO with full multi-sample
;support as you would see a lot fewer lines.
;Finally, the coolest thing you could do would be to create a normal map
;and manipulate the normal map such that you got better wave looks without
;millions of more polygons.
(defn do_display_wave_demo [drawable wave_proggy wave_time wave_width wave_height]
  (let [prog_hdl (wave_proggy :gl_handle)
	#^GL real_gl (. drawable getGL)
	#^GL gl (DebugGL. real_gl)
	#^GLU glu (GLU.)]
    (. gl glUseProgram prog_hdl)
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
    ;Draw here a plain surface
    (. gl glBegin GL/GL_QUADS)
    (dorun ;10,000 gl vertex 2f calls.  Not the most elegant way of doing it.
     (for [i (range (float -50) (float 50)) j (range (float -50) (float 50))] 
       (do
	 (. gl glVertex2f i j)
	 (. gl glVertex2f (+ i 1) j)
	 (. gl glVertex2f (+ i 1) (+ j 1))
	 (. gl glVertex2f i  (+ j 1)))))
    (. gl glEnd )
    (. gl glUseProgram 0)))

(defn display_wave_demo [drawable render_context_ref wave_time wave_width wave_height]
  (let [gl (. drawable getGL)
	wave_proggy (rcgl_get_glsl_program @render_context_ref "wave")]
    (. gl glClearColor 0.0 0.0 0.0 1.0)
    (. gl glClear GL/GL_COLOR_BUFFER_BIT)
    ;clear no matter what.  If the program is loaded, display the data.
    (when wave_proggy
      (do_display_wave_demo drawable wave_proggy wave_time wave_width wave_height))))

(def wave_frequency_ref (ref 1.0))
(defn set_wave_frequency [val] (dosync (ref-set wave_frequency_ref val)))

(def wave_width_ref (ref 0.1))
(defn set_wave_width [val] (dosync (ref-set wave_width_ref val)))

(def wave_height_ref (ref 3.0))
(defn set_wave_height [val] (dosync (ref-set wave_height_ref val)))

(defn display_animating_wave_demo [drawable render_context_ref start_milliseconds]
  (let [current (System/currentTimeMillis)
	wave_frequency @wave_frequency_ref
	relative (- current start_milliseconds)
	relative_seconds (/ relative 1000)
	rel_seq_freq (* relative_seconds wave_frequency)
	wave_time (rem rel_seq_freq Math/PI)]
    (display_wave_demo drawable render_context_ref wave_time @wave_width_ref @wave_height_ref)))
  

(def fm (ui_create_app_frame "wave_demo"))
;the resize has to come before the call to load the glsl program.
;this is because sometimes the jpanel jettisons the gl context
;thus invalidating all of your gl handles on resize.  Handling
;this gracefully is an ongoing task.
(. (fm :frame) setSize 400 300) 

(defn enable_animating_wave_demo []
  (load_wave_program fm)
  (let [current_millis (System/currentTimeMillis)
	rc_ref (ui_get_rcgl_render_context_ref fm)]
    (ui_set_gl_render_fn fm #(display_animating_wave_demo % rc_ref current_millis))
    (ui_set_fps_animator fm 60))
  nil)

(defn disable_animating_wave_demo []
  (ui_set_gl_render_fn fm nil)
  (delete_wave_program fm)
  (ui_disable_fps_animator fm)
  nil)

(println "(enable_animating_wave_demo)")
(println "(set_wave_frequency 5)")
(println "(set_wave_width 0.2)")
(println "(set_wave_height 5.0)")
(println "(disable_animating_wave_demo)")

(enable_animating_wave_demo)