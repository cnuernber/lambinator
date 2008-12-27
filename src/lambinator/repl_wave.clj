(use 'lambinator.ui)
(use 'lambinator.rcgl)
(import '(java.io File))
(import '(javax.media.opengl GL DebugGL))
(import '(javax.media.opengl.glu GLU))

(defn test_load_wave_glslv [drawable]
  (let [gl (. drawable getGL)
	file (File. "../data/glsl/wave.glslv")]
    (test_create_glsl_shader_from_file gl (. file getCanonicalPath))))
(defn test_load_wave_glslf [drawable]
  (let [gl (. drawable getGL)
	file (File. "../data/glsl/wave.glslf")]
    (test_create_glsl_shader_from_file gl (. file getCanonicalPath))))

(defn test_create_wave_proggy [drawable]
  (let [vs (test_load_wave_glslv drawable)
	fs (test_load_wave_glslf drawable)
	gl (. drawable getGL)]
    (create_glsl_program gl vs fs "wave_proggy")))

;creates an opengl program from the wave proggy
(defn load_wave_proggy[fm]
  (ui_run_sync_delayed_gl_function fm test_create_wave_proggy))

;runs one display loop of the wave demo.
;for a good time, remove the type inferencing support and see how long it takes...
;You could make this a *lot* faster by switching the vertex shader to use
;a vertex array attribute and calling glDrawArrays.
;Finally, it would be good to get this running on an FBO with full multi-sample
;support as you would see a lot fewer lines.
;Finally, the coolest thing you could do would be to create a normal map
;and manipulate the normal map such that you got better wave looks without
;millions of more polygons.
(defn display_wave_demo [drawable wave_proggy wave_time wave_width wave_height]
  (let [prog_hdl (wave_proggy :gl_handle)
	#^GL real_gl (. drawable getGL)
	#^GL gl (DebugGL. real_gl)
	#^GLU glu (GLU.)]
    (. gl glUseProgram prog_hdl)
    (. gl glClearColor 0.0 0.0 0.0 1.0)
    (. gl glClear GL/GL_COLOR_BUFFER_BIT)
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

;You can run whatever opengl commands you want to run using the ui_run_sync_delayed_gl_function
;The function gets passed a drawable (jogl glAutoDrawable I think)
(defn run_display_wave_demo[fm wave_proggy wave_time wave_width wave_height]
  (let [gl_fn (fn [drawable]
		(display_wave_demo drawable wave_proggy wave_time wave_width wave_height))]
    (ui_run_sync_delayed_gl_function fm gl_fn)))

(def fm (ui_create_app_frame "wave_demo"))
;the resize has to come before the call to load the glsl program.
;this is because sometimes the jpanel jettisons the gl context
;thus invalidating all of your gl handles on resize.  Handling
;this gracefully is an ongoing task.
(. (fm :frame) setSize 400 300) 
(def wp (load_wave_proggy fm))
(run_display_wave_demo fm wp 0.0 0.1 3.0)
;;OK, that was cool.  Now check out:
(defn run_display_wave_demo_a_lot []
  (doseq [wave_time (range (float 0.0) (float 50.0) (float 0.1))]
    (run_display_wave_demo fm wp wave_time 0.1 3.0)))
;If you resize the frame too much, you will need to re-create the shader program
;Also, be sure to check out your opengl extensions by looking at the about
;menu.
(println "(run_display_wave_demo_a_lot) ;do this")

