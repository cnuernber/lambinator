(load-file "repl_wave_defs.clj")

;generate a set of nexted vectors, one for each quad.
;flatten this out into straight up vectors
(defn repl_wave_geom_generator[]
  (map 
   float 
   (flatten 
    (for [i (range (float -50) (float 50)) j (range (float -50) (float 50))]
      [i j
       (+ i 1) j
       (+ i 1) (+ j 1)
       i (+ j 1)]))))

(defn create_wave_vbo[fm]
  (let [rc_ref (ui_get_rcgl_render_context_ref fm)
	render_tasks_ref (ui_get_render_todo_list_ref fm)]
    (rcgl_create_vbo rc_ref render_tasks_ref "wave_data" :data repl_wave_geom_generator)))

(defn delete_wave_vbo[fm]
  (let [rc_ref (ui_get_rcgl_render_context_ref fm)
	render_tasks_ref (ui_get_render_todo_list_ref fm)]
    (rcgl_delete_vbo rc_ref render_tasks_ref "wave_data" )))

(defn load_wave_resources [fm]
  (load_wave_program fm)
  (create_wave_vbo fm))

(defn delete_wave_resoures [fm]
  (delete_wave_program fm)
  (delete_wave_vbo fm))

;wave_program is a glsl_program
;wave_data is a gl_vbo
(defn do_display_wave_demo [drawable wave_proggy wave_data wave_time wave_width wave_height]
  (let [prog_hdl (wave_proggy :gl_handle)
	#^GL real_gl (. drawable getGL)
	#^GL gl (DebugGL. real_gl)
	#^GLU glu (GLU.)
	num_quads (/ (wave_data :item_count) 8)]
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

    (. gl glEnableClientState GL/GL_VERTEX_ARRAY)
    (. gl glBindBuffer (vbo_gl_type_from_vbo_type (wave_data :type)) (wave_data :gl_handle))
    (. gl glVertexPointer (int 2) (int (wave_data :gl_datatype)) (int 0) (long 0))
    (. gl glDrawArrays GL/GL_QUADS 0 (* num_quads 8))
    (. gl glDisableClientState GL/GL_VERTEX_ARRAY)
    (. gl glBindBuffer (vbo_gl_type_from_vbo_type (wave_data :type)) 0)
    (. gl glUseProgram 0)))

(defn display_wave_demo [drawable render_context_ref wave_time wave_width wave_height]
  (let [gl (. drawable getGL)
	wave_proggy (rcgl_get_glsl_program @render_context_ref "wave")
	wave_data (rcgl_get_vbo @render_context_ref "wave_data")]
    (. gl glClearColor 0.0 0.0 0.0 1.0)
    (. gl glClear GL/GL_COLOR_BUFFER_BIT)
    ;clear no matter what.  If the program is loaded, display the data.
    (if (and wave_proggy wave_data)
      (do_display_wave_demo drawable wave_proggy wave_data wave_time wave_width wave_height)
      (println "couldn't run due to missing resources"))))

(load-file "repl_wave_finish.clj")