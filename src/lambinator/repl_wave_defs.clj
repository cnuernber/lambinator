(use 'lambinator.ui)
(use 'lambinator.rcgl)
(use 'clojure.contrib.seq-utils)
(import '(java.io File))
(import '(javax.media.opengl GL DebugGL))
(import '(javax.media.opengl.glu GLU))

;given a frame, load the wave glsl* files
(defn load_wave_program [fm]
  (let [rc_ref (ui_get_rcgl_render_context_ref fm)
	render_tasks_ref (ui_get_render_todo_list_ref fm)]
    (rcgl_create_glsl_program 
     rc_ref 
     render_tasks_ref 
     "../data/glsl/wave.glslv" 
     "../data/glsl/wave.glslf"
     "wave")))

(defn delete_wave_program [fm]
  (let [rc_ref (ui_get_rcgl_render_context_ref fm)
	render_tasks_ref (ui_get_render_todo_list_ref fm)]
    (rcgl_delete_glsl_program rc_ref render_tasks_ref "wave")))


(def wave_frequency_ref (ref 1.0))
(defn set_wave_frequency [val] (dosync (ref-set wave_frequency_ref val)))

(def wave_width_ref (ref 0.1))
(defn set_wave_width [val] (dosync (ref-set wave_width_ref val)))

(def wave_height_ref (ref 3.0))
(defn set_wave_height [val] (dosync (ref-set wave_height_ref val)))