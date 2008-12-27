(ns lambinator.ui
  (:import (javax.media.opengl GLJPanel GLEventListener GL)
	   (javax.swing JFrame JMenu JMenuBar JMenuItem UIManager JDialog JLabel
			JScrollPane ScrollPaneConstants JTextField JTextPane)
	   (javax.swing.text.html HTMLEditorKit)
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
(defstruct ui_gl_win_data :gl_win :gl_system_strs_ref 
	   :gl_todo_list_ref :gl_dimensions_ref
	   :render_exceptions_ref :render_context_ref )


;creates all necessary ref's structs and returns a 
;win data that contains a live glgpanel	
;[render_exceptions_ref gl_dimensions_ref 
; gl_system_strs_ref gl_todo_items_ref]
(defn ui_create_gl_win_data[]
  (let [gl_system_strs_ref (ref nil)
	gl_todo_list_ref (ref nil)
	gl_dimensions_ref (ref [0 0 0 0])
	render_exceptions_ref (ref nil)
	render_context_ref (ref (create_render_context))
	panel (GLJPanel.)
	listener (create_gl_event_listener 
		  render_exceptions_ref 
		  gl_dimensions_ref 
		  gl_system_strs_ref 
		  gl_todo_list_ref
		  render_context_ref)]
    (. panel addGLEventListener listener)
    (struct ui_gl_win_data 
	    panel
	    gl_system_strs_ref
	    gl_todo_list_ref
	    gl_dimensions_ref
	    render_exceptions_ref
	    render_context_ref)))

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

(defn ui_get_render_todo_lists_ref[frame_data]
  (let [{ { retval :gl_todo_list_ref } :win_data } frame_data]
    retval))