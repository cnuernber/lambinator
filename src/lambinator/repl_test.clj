(use 'lambinator.ui)
(use 'lambinator.rcgl)
(import '(java.io File))
(import '(javax.media.opengl GL DebugGL))
(import '(javax.media.opengl.glu GLU))

(defn test_load_wave_glslv [drawable]
  (let [gl (. drawable getGL)
	file (File. "../data/glsl/wave.glslv")]
    (test_create_glsl_script_from_file gl (. file getCanonicalPath))))
(defn test_load_wave_glslf [drawable]
  (let [gl (. drawable getGL)
	file (File. "../data/glsl/wave.glslf")]
    (test_create_glsl_script_from_file gl (. file getCanonicalPath))))

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
    (dorun
     (for [i (range (float -50) (float 50)) j (range (float -50) (float 50))] 
       (do
	 (. gl glVertex2f i j)
	 (. gl glVertex2f (+ i 1) j)
	 (. gl glVertex2f (+ i 1) (+ j 1))
	 (. gl glVertex2f i  (+ j 1)))))
    (. gl glEnd )
    (. gl glUseProgram 0)))


(defn run_display_wave_demo[fm wave_proggy wave_time wave_width wave_height]
  (let [gl_fn (fn [drawable]
		(display_wave_demo drawable wave_proggy wave_time wave_width wave_height))]
    (ui_run_sync_delayed_gl_function fm gl_fn)))