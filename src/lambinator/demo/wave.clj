(ns lambinator.demo.wave
  (:use lambinator.ui lambinator.rcgl lambinator.rc
	clojure.contrib.seq-utils lambinator.util
	lambinator.log lambinator.ui.inspector)
  (:import (java.io File)
	  (javax.media.opengl GL DebugGL)
	  (javax.media.opengl.glu GLU)))

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

(defn general_display_wave_demo[render_context_ref drawable wave_proggy display_predicate 
				wave_time wave_width wave_height geom_fn]
  (let [#^GL real_gl (. drawable getGL)
	#^GL gl (DebugGL. real_gl)
	#^GLU glu (GLU.)
	logger_ref (@render_context_ref :logger_ref)]
    (. gl glClearColor 0.0 0.0 0.2 1.0)
    (. gl glClear GL/GL_COLOR_BUFFER_BIT)
    (if (display_predicate)
      (do
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
	(rcgl_set_glsl_uniforms 
	 @render_context_ref
	 gl
	 [["waveTime" wave_time]
	  ["waveWidth" wave_width]
	  ["waveHeight" wave_height]]
	 wave_proggy )
	(geom_fn gl)
	(. gl glUseProgram 0))
      (log_message @logger_ref "wave demo:" :info "Missing resources for render"))))

  
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
     render_context_ref
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

(defn create_wave_drawable_fn[wave_demo_data_ref demo_fn]
  (let [fm (@wave_demo_data_ref :frame)
	current_millis (System/currentTimeMillis)
	rc_ref (ui_get_rcgl_render_context_ref fm)]
    #(display_animating_wave_demo % rc_ref wave_demo_data_ref current_millis demo_fn)))
  

(defn setup_wave_demo[wave_demo_data_ref demo_fn]
  (let [fm (@wave_demo_data_ref :frame)]
    (ui_set_gl_render_fn fm (create_wave_drawable_fn wave_demo_data_ref demo_fn))
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
     render_context_ref
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

(defn generate_multisample_vbo[]
  (map 
   float
   [-1 -1  0 0
    -1  1  0 1
     1  1  1 1
     1 -1  1 0]))

(defonce aa_choices_array [:none :2 :4 :8 :16])

(defn create_multisample_fbos[wave_demo_data_ref]
  (let [{ { { drawable :gl_win } :win_data } :frame } @wave_demo_data_ref
	width (. drawable getWidth)
	height (. drawable getHeight)
	num_samples (@wave_demo_data_ref :num_samples)
	indexed_choices_array (map vector (iterate inc 0) (rest aa_choices_array))
	[this_choice_index _] (first (filter (fn [[index choice]] (= choice num_samples)) indexed_choices_array))
	;pick aa modes that are less than or equal to the chosen aa mode so we have valid fallbacks
	valid_choices (reverse (filter (fn [[index choice]] (>= this_choice_index index)) indexed_choices_array))
	ms_spec_seq (map (fn [[index choice]]
			   (create_surface_spec 
			    { :color0 (create_renderbuffer :color :ubyte :rgb false) }
			    width
			    height
			    choice))
			 valid_choices )
	trans_spec (create_surface_spec
		    { :color0 (create_renderbuffer :color :ubyte :rgb  true) }
		    width
		    height)]
    (with_context_and_tasks_refs 
     wave_demo_data_ref
     (fn [rc rl]
       (rcgl_create_context_surface_seq rc rl ms_spec_seq "wave_multisample_surface")
       (rcgl_create_context_surface rc rl trans_spec "wave_transfer_surface")))))

(defn create_multisample_data[wave_demo_data_ref]
  (create_multisample_fbos wave_demo_data_ref)
  (with_context_and_tasks_refs 
   wave_demo_data_ref
   (fn [rc rl]
     (rcgl_create_glsl_program 
      rc 
      rl
      "../data/glsl/passthrough.glslv" 
      "../data/glsl/single_texture.glslf"
      "wave_final_render_prog")
     (rcgl_create_vbo rc rl "wave_multisample_vbo" :data #(generate_multisample_vbo)))))

(defn delete_multisample_data[wave_demo_data_ref]
  (with_context_and_tasks_refs 
   wave_demo_data_ref
   (fn [rc rl]
     (rcgl_delete_context_surface rc rl "wave_multisample_surface")
     (rcgl_delete_context_surface rc rl "wave_transfer_surface")
     (rcgl_delete_glsl_program rc rl "wave_final_render_prog")
     (rcgl_delete_vbo rc rl "wave_multisample_vbo"))))

(defn antialiasing_drawable_wrapper[drawable render_context_ref frame_resize_data wave_demo_data_ref child_drawable]
  (let [real_gl (. drawable getGL)
	#^GL gl (DebugGL. real_gl)
	width (. drawable getWidth)
	height (. drawable getHeight)
	render_context @render_context_ref
	ms_surface (rcgl_get_context_surface render_context "wave_multisample_surface")
	transfer_surface (rcgl_get_context_surface render_context "wave_transfer_surface")
	final_prog (rcgl_get_glsl_program render_context "wave_final_render_prog")
	ms_vbo (rcgl_get_vbo render_context "wave_multisample_vbo")
	do_aa_render (and ms_surface transfer_surface final_prog child_drawable ms_vbo)]
    (if do_aa_render
      (let [ms_fbo (ms_surface :gl_handle)
	    transfer_fbo (transfer_surface :gl_handle)
	    prog_handle (final_prog :gl_handle)
	    transfer_tex (((transfer_surface :attachments) :color0) :texture_gl_handle)
	    tex_att_index (((final_prog :attributes) "input_tex_coords") :index)
	    [render_width render_height] ((ms_surface :surface_spec) :size)
	    vbo_dtype (int (ms_vbo :gl_datatype))]
	;have the child render to the multisample fbo
	;the bind function sets where gl will render to
	(. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT ms_fbo)
	(try
	 (. gl glViewport 0 0 render_width render_height)
	 (child_drawable drawable)
					;bind the multisample framebuffer as the read framebuffer source
	 (. gl glBindFramebufferEXT GL/GL_READ_FRAMEBUFFER_EXT ms_fbo)
					;bind the transfer as the draw framebuffer dest
	 (. gl glBindFramebufferEXT GL/GL_DRAW_FRAMEBUFFER_EXT transfer_fbo);
        ;downsample the multisample fbo to the draw framebuffer's texture
	 (. gl glBlitFramebufferEXT 
	    0 0 render_width render_height ;source rect
	    0 0 render_width render_height ;dest rect
	    GL/GL_COLOR_BUFFER_BIT ;what to copy over (just color in our case)
	    GL/GL_NEAREST ) ;how to interpolate intermediate results (there aren't any; the sizes match)

        ;Bind the window's render surface as the target render surface
	 (. gl GL/glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT 0)
	 (. gl glViewport 0 0 width height)
	;Now we render our fullscreen quad
	 (. gl glShadeModel GL/GL_SMOOTH)
	 (. gl glPolygonMode GL/GL_FRONT_AND_BACK GL/GL_FILL)
	 (. gl glMatrixMode GL/GL_PROJECTION)
	 (. gl glLoadIdentity )
	 (. gl glMatrixMode GL/GL_MODELVIEW)
	 (. gl glLoadIdentity)
	 (. gl glUseProgram prog_handle)
	 (. gl glEnableClientState GL/GL_VERTEX_ARRAY)
	 (. gl glEnableVertexAttribArray tex_att_index)
	 (. gl glBindBuffer (vbo_gl_type_from_vbo_type (ms_vbo :type)) (ms_vbo :gl_handle))
	 (. gl glEnable GL/GL_TEXTURE_2D)
	 (. gl glActiveTexture GL/GL_TEXTURE0)
	 (. gl glBindTexture GL/GL_TEXTURE_2D transfer_tex)
	 (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_MIN_FILTER GL/GL_LINEAR)
	 (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_MAG_FILTER GL/GL_LINEAR)
	 (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_WRAP_S GL/GL_CLAMP_TO_EDGE)
	 (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_WRAP_T GL/GL_CLAMP_TO_EDGE)
        ;we have how bound the second set of texture coordinates to tex coord 0
	;each tex coord takes two entries, they have a stride of 4
	;and they are offset from the beginning of the array by two
	 (. gl glVertexAttribPointer 
	    tex_att_index ;index
	    (int 2)       ;size
	    vbo_dtype     ;type
	    false         ;normalized
	    (int 16)       ;stride
	    (long 8))      ;offset
	 (rcgl_set_glsl_uniforms
	  @render_context_ref
	  gl
	  [["tex" 0]] ;set the texture param to desired logical texture unit
	  final_prog )
        ; Render Fullscreen Quad
	 (. gl glVertexPointer (int 2) (int (ms_vbo :gl_datatype)) (int 16) (long 0))
					;glDrawArrays takes the index count, not the polygon count or the array item count
	 (. gl glDrawArrays GL/GL_QUADS 0 (/ (ms_vbo :item_count) 4)) ;each index has an x and y, u and v
	 (. gl glDisableClientState GL/GL_VERTEX_ARRAY)
	 (. gl glActiveTexture GL/GL_TEXTURE0)
	 (. gl glBindBuffer (vbo_gl_type_from_vbo_type (ms_vbo :type)) 0)
	 (finally
	  (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT 0)))) ;make goddamn sure we don't end up with an invalid fbo bound.
      
      (when child_drawable ;if we can't render antialiased because we don't have buffers
	(child_drawable drawable)))
    ;update frame resize data so we know how many times the drawable has rendered at this exact size
    ;we only resize when a certain number of frames have been rendered at a certain size.
    ;this is because resizing fbos is relative expensive and can apparently lead
    ;to fragmentation of video ram (although I doubt the second claim)
    (let [resize_frame_count (@frame_resize_data :resize_frame_count)
	  [rs_width rs_height] (@frame_resize_data :resize_frame_size)
	  resize_frame_count (if (and (== rs_width width)
				      (== rs_height height))
			       (inc resize_frame_count)
			       0)
	  fbos_missing (or (not ms_surface)
			   (not transfer_surface))
	  fbos_size_mismatch (or fbos_missing
				 (not (= ((ms_surface :surface_spec) :size)
					 [width height])))
	  do_resize_fbo (or fbos_missing
			    (and fbos_size_mismatch
				 (> resize_frame_count 10)))]
      (dosync (ref-set frame_resize_data (assoc @frame_resize_data 
					   :resize_frame_count resize_frame_count
					   :resize_frame_size [width height])))
      (when do_resize_fbo
	(create_multisample_fbos wave_demo_data_ref)))
    (when ms_surface
      (let [actual_sample_count ((ms_surface :surface_spec) :multi_sample)]
	(when (not (= (@wave_demo_data_ref :num_samples)
		      actual_sample_count))
	  (dosync (ref-set wave_demo_data_ref (assoc @wave_demo_data_ref :num_samples actual_sample_count)))
	  ;update ui to reflect reality which, due to differenes
	  ;in hardware, may not match what the user wanted.
	  (let [item (first (filter 
			     (fn [item] (= (item :name) "Antialiasing: "))
			     (@wave_demo_data_ref :inspector_items)))]
	    (when item
	      ((item :updater)))))))))

(defmulti get_wave_demo_fn (fn [demo_ref] (@demo_ref :geom_type)))
(defmethod get_wave_demo_fn :default [_] display_vbo_wave_demo)
(defmethod get_wave_demo_fn :immediate [_] display_simple_wave_demo)

(defn create_aa_drawable_fn[wave_demo_data_ref]
  (let [frame_resize_data (ref {:resize_frame_count 0 :resize_frame_size [0 0]})
	fm (@wave_demo_data_ref :frame)
	rc_ref ((fm :win_data) :render_context_ref)
	drawable_fn (create_wave_drawable_fn wave_demo_data_ref (get_wave_demo_fn wave_demo_data_ref))
	aa_drawable_fn #(antialiasing_drawable_wrapper % rc_ref frame_resize_data wave_demo_data_ref drawable_fn)]
    aa_drawable_fn))
  

(defn enable_antialiased_wave_demo [wave_demo_data_ref]
  (let [drawable_fn (create_aa_drawable_fn wave_demo_data_ref)
	fm (@wave_demo_data_ref :frame)]
    (load_wave_program wave_demo_data_ref)
    (create_wave_vbo wave_demo_data_ref)
    (create_multisample_data wave_demo_data_ref)
    (ui_set_gl_render_fn fm drawable_fn)
    (ui_set_fps_animator fm 60)))

(defn disable_wave_demo[wave_demo_data_ref]
  (let [fm (@wave_demo_data_ref :frame)]
    (ui_set_gl_render_fn fm nil)
    (ui_set_fps_animator fm 5) ;just ensure the window refreshes regularly
    (delete_wave_program wave_demo_data_ref)
    (delete_wave_vbo wave_demo_data_ref)
    (delete_multisample_data wave_demo_data_ref)
    nil))

(defonce geom_choices_array [:immediate :vbo])
(defstruct wave_demo_data :frame :wave_freq :wave_width :wave_height :num_samples :geom_type :inspector_items)

(defn reset_wave_demo[demo_data_ref]
  (load_wave_program demo_data_ref)
  (create_wave_vbo demo_data_ref)
  (let [drawable_fn (if (= (@demo_data_ref :num_samples) :none)
		      (create_wave_drawable_fn demo_data_ref (get_wave_demo_fn demo_data_ref))
		      (do
			(create_multisample_data demo_data_ref)
			(create_aa_drawable_fn demo_data_ref)))
	fm (@demo_data_ref :frame)]
    (ui_set_gl_render_fn fm drawable_fn)
    (ui_set_fps_animator fm 60)))
	

(defn create_wave_demo[]
  (let [frame (ui_create_app_frame "wave demo")
	retval (ref (struct-map wave_demo_data
		      :frame frame
		      :wave_freq 1.0
		      :wave_width 0.1
		      :wave_height 3.0
		      :num_samples :4
		      :geom_type :vbo))
	aa_item (create_list_inspector_item 
		 "Antialiasing: " ;item name
		 aa_choices_array ;choices
		 (fn [] (@retval :num_samples)) ;getter
		 (fn [val] 
		   (dosync (ref-set retval (assoc @retval :num_samples val)))
		   (reset_wave_demo retval)) ;setter
		 (fn [item]
		   (if (= item :none)
		     "none"
		     (stringify (name item) "x"))))
	geom_item (create_list_inspector_item
		   "Geom Render Mode: " ;name
		   geom_choices_array   ;options 
		   (fn [] (@retval :geom_type)) ;getter
		   (fn [val] (dosync (ref-set retval (assoc @retval :geom_type val)))
		     (reset_wave_demo retval)) ;setter
		   (fn [item] ;stringify
		     (if (= item :immediate)
		       "immediate"
		       "vertex buffer object"
		       )))
	freq_item (create_float_slider_inspector_item
		   "Wave Frequency: "
		   0
		   30
		   (fn [] (@retval :wave_freq))
		   (fn [val] (dosync (ref-set retval (assoc @retval :wave_freq val))))
		   "00.0")
	width_item (create_float_slider_inspector_item
		    "Wave Width: "
		    0
		    1
		   (fn [] (@retval :wave_width))
		   (fn [val] (dosync (ref-set retval (assoc @retval :wave_width val))))
		   "0.00")
	height_item (create_float_slider_inspector_item
		     "Wave Height: "
		     0
		     100
		     (fn [] (@retval :wave_height))
		     (fn [val] (dosync (ref-set retval (assoc @retval :wave_height val))))
		     "000.0")
	inspector_items [aa_item geom_item freq_item width_item height_item]]
    (dosync (ref-set retval (assoc @retval :inspector_items inspector_items)))
    (setup_inspector_panel (frame :inspector_pane) inspector_items)
    (reset_wave_demo retval)
    (. (frame :frame) setVisible true)
    retval))