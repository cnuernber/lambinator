(load-file "repl_wave_defs.clj")

(defn load_wave_resources [fm]
  (load_wave_program fm))

(defn delete_wave_resoures [fm]
  (delete_wave_program fm))

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

(load-file "repl_wave_finish.clj")