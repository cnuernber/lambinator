(ns lambinator.ui
  (:import (javax.media.opengl GLJPanel GLEventListener GL GLCapabilities)
	   (javax.swing JFrame JMenu JMenuBar JMenuItem UIManager JDialog JLabel
			JScrollPane ScrollPaneConstants JTextField JTextPane
			SwingUtilities JButton JPanel)
	   (javax.swing.text.html HTMLEditorKit)
	   (com.sun.opengl.util FPSAnimator)
	   (java.awt BorderLayout GridBagLayout GridBagConstraints Dimension)
	   (java.awt.event ActionListener)
	   (java.util.regex Pattern)
	   (java.util.concurrent CountDownLatch)
	   (org.noos.xing.mydoggy ToolWindow ToolWindowAnchor ToolWindowManager)
	   (org.noos.xing.mydoggy.plaf MyDoggyToolWindowManager)) ;forcing async to sync
  (:use lambinator.util lambinator.rcgl lambinator.log
	clojure.contrib.seq-utils))

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
;you pass in a javax.media.opengl.GLCapabilities
;win data that contains a live glgpanel	
;[render_exceptions_ref gl_dimensions_ref 
; gl_system_strs_ref gl_todo_items_ref]
(defn ui_create_gl_win_data[glcapabilities]
  (let [gl_system_strs_ref (ref nil)
	;this list gets run once then cleared
	gl_todo_list_ref (ref nil)
	gl_dimensions_ref (ref [0 0 0 0])
	render_exceptions_ref (ref nil)
	logger (ref (create_log_data))
	render_context_ref (ref (create_render_context logger))
	gl_render_fn_ref (ref nil)
	panel (GLJPanel. glcapabilities)
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

(defstruct ui_frame_data :frame :win_data :log_pane :inspector_pane :log_messages_ref :log_message_len)

(defn init_tool_window_manager[]
  (let [window_mgr (MyDoggyToolWindowManager.)
	log_pane (create_label "")
	newTool (JScrollPane. log_pane)
	newPanel (JPanel.)
	weird_palette (create_label "Palette so my app doesn't look weird" )]
    
    (. window_mgr registerToolWindow "Log" "Application Log" nil newTool ToolWindowAnchor/BOTTOM)
    (. window_mgr registerToolWindow "Inspector" "Inspector Palette" nil newPanel ToolWindowAnchor/RIGHT)
    (. window_mgr registerToolWindow "Anti-Weird" "Anti-Weird Palette" nil weird_palette ToolWindowAnchor/LEFT)
    (doseq [window (. window_mgr getToolWindows)]
      (. window setAvailable true))
    [window_mgr log_pane newPanel]))

(defn ui_add_log_message[frame_data module type & args]
  (let [log_window (frame_data :log_pane)
	agent (agent {})]
    (send agent 
     (fn [_]
       (try
	(let [log_messages_ref (frame_data :log_messages_ref)
	      args_str_lines (split_on_newline (apply stringify (concat (list module " " (name type) ": ") (flatten args))))
	      new_message_list (take (frame_data :log_message_len) (concat args_str_lines @log_messages_ref))
	      builder (StringBuilder.)]
	  (dosync (ref-set log_messages_ref new_message_list))
	  (. builder append "<html>")
	  (doseq [msg (reverse new_message_list)]
	    (. builder append msg)
	    (. builder append "<br>\n"))
	  (. builder append "</html>")
	  (SwingUtilities/invokeLater 
	   (fn []
	     (. log_window setText (. builder toString))))
	  nil)
	(catch Exception e
	  (. e printStackTrace)
	 nil))))))
	
(defn ui_create_app_frame 
  ([appName capabilities]
     (. (System/getProperties) setProperty "apple.laf.useScreenMenuBar" "true")
     (. (System/getProperties) setProperty "com.apple.mrj.application.apple.menu.about.name" appName)
     (let [frame (JFrame. appName)
	   { panel :gl_win gl_system_strs_ref :gl_system_strs_ref 
	    render_context_ref :render_context_ref :as win_data } ( ui_create_gl_win_data capabilities)
	   logger_ref (@render_context_ref :logger_ref)
	   bar (JMenuBar.)
	   menu (JMenu. "About")
	   [window_mgr log_label inspector] (init_tool_window_manager)]
       (.. frame getContentPane (setLayout (BorderLayout.)))
       (.. frame getContentPane (add window_mgr))
       (. (. window_mgr getContentManager) addContent "gl_panel" nil nil panel)
       (. bar add menu)
       (create_menu_item "OpenGL" (fn [ignored] (display_opengl_properties frame gl_system_strs_ref)) menu)
       (. frame setJMenuBar bar)
       (. frame setSize 1000 600)
       (SwingUtilities/invokeLater 
	(fn [] 
	  (. (. window_mgr getToolWindow "Log") setActive true)
	  (. (. window_mgr getToolWindow "Inspector") setActive true)
	  (. (. window_mgr getToolWindow "Anti-Weird") setActive true)
	  (. frame setVisible true)))
       (let [retval (struct ui_frame_data frame win_data log_label inspector (ref nil) 1000)]
	 (dosync (ref-set logger_ref (log_add_listener @logger_ref 
						       (fn [module type & args]
							 (ui_add_log_message retval module type args)))))
	 retval)))
  ([appName] (ui_create_app_frame appName nil)))

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

(defn ui_get_render_todo_list_ref [frame_data]
  (let [{ { retval :gl_todo_list_ref } :win_data } frame_data]
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

(defn ui_disable_fps_animator[frame_data]
  (let [{ { animator_ref :animator_ref :as win_data } :win_data } frame_data
	old_animator @animator_ref]
    (when old_animator
      (. old_animator stop))
    (dosync (ref-set animator_ref nil))))

(defn ui_set_fps_animator[frame_data inFPS]
  (let [{ { drawable :gl_win animator_ref :animator_ref } :win_data } frame_data
	animator (FPSAnimator. drawable inFPS true)
	old_animator @animator_ref]
    (ui_disable_fps_animator frame_data)
    (. animator start)
    (dosync (ref-set animator_ref animator))))

(defn ui_update_display [frame_data]
  (let [{ { panel :gl_win } :win_data } frame_data
	dims (. panel getSize)]
    (. panel repaint 0 0 (.width dims) (.height dims))))
  
    