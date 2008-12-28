(ns lambinator.ui
  (:import (javax.media.opengl GLJPanel GLEventListener GL)
	   (javax.swing JFrame JMenu JMenuBar JMenuItem UIManager JDialog JLabel
			JScrollPane ScrollPaneConstants JTextField JTextPane)
	   (javax.swing.text.html HTMLEditorKit)
	   (com.sun.opengl.util FPSAnimator)
	   (java.awt BorderLayout GridBagLayout GridBagConstraints Dimension)
	   (java.awt.event ActionListener)
	   (java.util.regex Pattern)
	   (java.util.concurrent CountDownLatch)) ;forcing async to sync
  (:use lambinator.util lambinator.rcgl))

(load "ui_defs")

;gl_win points to the actual swing jglwindow
;gl_system_strs is a map of gl_system_lookup_str to value
;gl_todo_list is a list of items that will happen next time 
;the window updates 
;dimensions always contains the latest known dimensions.
;render_exceptions is a list of exceptions caught during the render loop.
;render_context_ref 
(defstruct ui_gl_win_data :gl_win ;GLJpanel
	   :gl_system_strs_ref  ;system data
	   :gl_todo_list_ref    ;todo functions, run once then cleared
	   :gl_dimensions_ref   ;currently known dimensions
	   :render_exceptions_ref ;exceptions caught during rendering
	   :render_context_ref ;reference to render context
	   :gl_render_fn_ref ;render function, run every display
	   :animator_ref ;ref to the animator item
	   )

;creates all necessary ref's structs and returns a 
;win data that contains a live glgpanel	
;[render_exceptions_ref gl_dimensions_ref 
; gl_system_strs_ref gl_todo_items_ref]
(defn ui_create_gl_win_data[]
  (let [gl_system_strs_ref (ref nil)
	;this list gets run once then cleared
	gl_todo_list_ref (ref nil)
	gl_dimensions_ref (ref [0 0 0 0])
	render_exceptions_ref (ref nil)
	render_context_ref (ref (create_render_context))
	gl_render_fn_ref (ref nil)
	panel (GLJPanel.)
	listener (create_gl_event_listener 
		  render_exceptions_ref 
		  gl_dimensions_ref 
		  gl_system_strs_ref 
		  gl_todo_list_ref
		  render_context_ref
		  gl_render_fn_ref)]
    (. panel addGLEventListener listener)
    (struct ui_gl_win_data 
	    panel
	    gl_system_strs_ref
	    gl_todo_list_ref
	    gl_dimensions_ref
	    render_exceptions_ref
	    render_context_ref
	    gl_render_fn_ref
	    (ref nil))))

(defstruct ui_frame_data :frame :win_data)

(defn ui_create_app_frame [appName]
  (. (System/getProperties) setProperty "apple.laf.useScreenMenuBar" "true")
  (. (System/getProperties) setProperty "com.apple.mrj.application.apple.menu.about.name" appName)
  (let [frame (JFrame. appName)
	{ panel :gl_win gl_system_strs_ref :gl_system_strs_ref :as win_data } ( ui_create_gl_win_data)
	bar (JMenuBar.)
	menu (JMenu. "About")]
    (.. frame getContentPane (setLayout (BorderLayout.)))
    (.. frame getContentPane (add panel))
    (. bar add menu)
    (create_menu_item "OpenGL" (fn [ignored] (display_opengl_properties frame gl_system_strs_ref)) menu)
    (. frame setJMenuBar bar)
    (. frame setSize 800 600)
    (. frame show)
    (struct ui_frame_data frame win_data)))

;add a todo item and tell the view to redraw itself
;drawable fn must take only one argument, that is the drawable
;this function calls invalidate on the glJPanel or does something
;to immediately schedule a render.
;If you want your function to keep rendering, then just have it keep calling
;this function with itself.
(defn ui_register_gl_drawable_todo_with_update [frame_data drawable_fn]
  (let [{ {gl_todo_list_ref :gl_todo_list_ref panel :gl_win } :win_data } frame_data]
    (add_gl_todo_item gl_todo_list_ref drawable_fn)
    (. panel display)))

;it is fine to set the drawable fn to nil.
(defn ui_set_gl_render_fn[frame_data drawable_fn]
  (let [{ {gl_render_fn_ref :gl_render_fn_ref panel :gl_win } :win_data } frame_data]
    (dosync (ref-set gl_render_fn_ref drawable_fn))))

;this runs a function on the render thread
;and returns the result, blocking the caller.
;if the result is an exception, it is wrapped and thrown
(defn ui_run_sync_delayed_gl_function[frame_data drawable_fn]
  (let [result_ref (ref nil)
	error_ref (ref nil)
	latch (CountDownLatch. 1)
	fn_wrapper (fn [drawable]
		     (try
		      (let [result (drawable_fn drawable)]
			(dosync (ref-set result_ref result)))
		      (catch Exception e
			(dosync (ref-set error_ref e)))
		      (finally
		       (. latch countDown))))]
    (ui_register_gl_drawable_todo_with_update frame_data fn_wrapper)
    (. latch await)
    (let [result @result_ref
	  error @error_ref]
      (if error
	(throw (Exception. error))
	result))))

(defn ui_run_gl_function_with_swap_and_update[frame_data drawable_fn]
  (let [fn_wrapper (fn [drawable]
		     (try
		      (drawable_fn drawable)
		      (catch Exception e)))]
    (ui_register_gl_drawable_todo_with_update frame_data fn_wrapper)))
  
(defn ui_get_rcgl_render_context_ref[frame_data]
  (let [{ { retval :render_context_ref } :win_data } frame_data]
    retval))

(defn animator_render_update [drawable ui_animator]
  (let [default_render_fn_ref (ui_animator :default_render_fn_ref)
	render_fn_ref (ui_animator :render_fn_ref)
	render_fn @render_fn_ref
	default_render_fn @default_render_fn_ref]
    (if render_fn
      (render_fn drawable)
      (if default_render_fn
	(default_render_fn drawable)
	(let [gl (. drawable getGL)]
	  (. gl glClearColor 0.2 0.2 1.0 1.0)
	  (. gl glClear GL/GL_COLOR_BUFFER_BIT))))))

(defn ui_set_fps_animator[frame_data inFPS]
  (let [{ { drawable :gl_win animator_ref :animator_ref } :win_data } frame_data
	animator (FPSAnimator. drawable inFPS true)
	old_animator @animator_ref]
    (when old_animator 
      (. old_animator stop))
    (. animator start)
    (dosync (ref-set animator_ref animator))))

(defn ui_disable_fps_animator[frame_data]
  (let [{ { animator_ref :animator_ref :as win_data } :win_data } frame_data
	old_animator @animator_ref]
    (when old_animator
      (. old_animator stop))
    (dosync (ref-set animator_ref nil))))
    