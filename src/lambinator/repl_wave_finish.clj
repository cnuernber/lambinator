(defn display_animating_wave_demo [drawable render_context_ref start_milliseconds]
  (let [current (System/currentTimeMillis)
	wave_frequency @wave_frequency_ref
	relative (- current start_milliseconds)
	relative_seconds (/ relative 1000)
	rel_seq_freq (* relative_seconds wave_frequency)
	wave_time (rem rel_seq_freq Math/PI)]
    (display_wave_demo drawable render_context_ref wave_time @wave_width_ref @wave_height_ref)))

(defonce fm nil)

(defn create_ui[]
  (def fm (ui_create_app_frame "wave_demo"))
;the resize has to come before the call to load the glsl program.
;this is because sometimes the jpanel jettisons the gl context
;thus invalidating all of your gl handles on resize.  Handling
;this gracefully is an ongoing task.
  (. (fm :frame) setSize 400 300) )

(defn enable_animating_wave_demo []
  (when (not fm)
    (create_ui))
  (load_wave_resources fm)
  (let [current_millis (System/currentTimeMillis)
	rc_ref (ui_get_rcgl_render_context_ref fm)]
    (ui_set_gl_render_fn fm #(display_animating_wave_demo % rc_ref current_millis))
    (ui_set_fps_animator fm 60))
  (println "Running demo at 60 fps")
  nil)

(defn disable_animating_wave_demo []
  (ui_set_gl_render_fn fm nil)
  (delete_wave_resoures fm)
  ;ensure we update the screen
  (ui_set_fps_animator fm 10) 
  nil)

(println "(enable_animating_wave_demo)")
(println "(set_wave_frequency 5)")
(println "(set_wave_width 0.2)")
(println "(set_wave_height 5.0)")
(println "(disable_animating_wave_demo)")

;(enable_animating_wave_demo)