(ns lambinator.ui
  (:import (javax.media.opengl GLJPanel GLEventListener GL GLCapabilities)
	   (javax.swing JFrame JMenu JMenuBar JMenuItem UIManager JDialog JLabel
			JScrollPane ScrollPaneConstants JTextField JTextPane
			SwingUtilities JButton JPanel)
	   (javax.swing.text.html HTMLEditorKit)
	   (com.sun.opengl.util FPSAnimator)
	   (java.awt BorderLayout GridBagLayout GridBagConstraints Dimension)
	   (java.awt.event ActionListener WindowListener ComponentListener)
	   (java.util.regex Pattern)
	   (java.util.concurrent CountDownLatch)
	   (java.util Timer TimerTask Date)
	   (org.noos.xing.mydoggy ToolWindow ToolWindowAnchor ToolWindowManager)
	   (org.noos.xing.mydoggy.plaf MyDoggyToolWindowManager)) ;forcing async to sync
  (:use lambinator.util lambinator.rcgl lambinator.log
	clojure.contrib.seq-utils
	lambinator.fs))

(load "ui_defs")

;gl-win points to the actual swing jglwindow
;gl-system-strs is a map of gl-system-lookup-str to value
;gl-todo-list is a list of items that will happen next time 
;the window updates 
;dimensions always contains the latest known dimensions.
;render-exceptions is a list of exceptions caught during the render loop.
;render-context-ref 
(defstruct ui-gl-win-data :gl-win ;GLJpanel
	   :gl-system-strs-ref  ;system data
	   :gl-todo-list-ref    ;todo functions, run once then cleared
	   :gl-dimensions-ref   ;currently known dimensions
	   :render-exceptions-ref ;exceptions caught during rendering
	   :render-context-ref ;reference to render context
	   :gl-render-fn-ref ;render function, run every display
	   :animator-ref ;ref to the animator item
	   )

;creates all necessary ref's structs and returns a 
;you pass in a javax.media.opengl.GLCapabilities
;win data that contains a live glgpanel	
;[render-exceptions-ref gl-dimensions-ref 
; gl-system-strs-ref gl-todo-items-ref]
(defn ui-create-gl-win-data[glcapabilities]
  (let [gl-system-strs-ref (ref nil)
	;this list gets run once then cleared
	gl-todo-list-ref (ref nil)
	gl-dimensions-ref (ref [0 0 0 0])
	render-exceptions-ref (ref nil)
	logger (ref (create-log-data))
	render-context-ref (ref (create-render-context logger))
	gl-render-fn-ref (ref nil)
	panel (GLJPanel. glcapabilities)
	listener (create-gl-event-listener 
		  render-exceptions-ref 
		  gl-dimensions-ref 
		  gl-system-strs-ref 
		  gl-todo-list-ref
		  render-context-ref
		  gl-render-fn-ref)]
    (. panel addGLEventListener listener)
    (struct ui-gl-win-data 
	    panel
	    gl-system-strs-ref
	    gl-todo-list-ref
	    gl-dimensions-ref
	    render-exceptions-ref
	    render-context-ref
	    gl-render-fn-ref
	    (ref nil))))

(defn init-tool-window-manager[]
  (let [window-mgr (MyDoggyToolWindowManager.)
	log-pane (create-label "")
	newTool (JScrollPane. log-pane)
	inspector-panel (JPanel.)
	inspector-scroll (JScrollPane. inspector-panel
				       ScrollPaneConstants/VERTICAL_SCROLLBAR_AS_NEEDED
				       ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)]
    
    (. window-mgr registerToolWindow "Log" "Application Log" nil newTool ToolWindowAnchor/BOTTOM)
    (. window-mgr registerToolWindow "Inspector" "Inspector Palette" nil inspector-scroll ToolWindowAnchor/RIGHT)
    (doseq [window (. window-mgr getToolWindows)]
      (. window setAvailable true))
    [window-mgr log-pane inspector-panel]))

(defn ui-add-log-message[frame-data module type & args]
  (let [log-window (frame-data :log-pane)
	agent (agent {})]
    (send agent 
     (fn [-]
       (try
	(let [log-messages-ref (frame-data :log-messages-ref)
	      args-str-lines (reverse (map (fn [line]
					     (stringify module " " (name type) ": " line))
					   (split-on-newline (apply stringify (flatten args)))))
	      new-message-list (take @(frame-data :log-length-ref) (concat args-str-lines @log-messages-ref))]
	  (dosync (ref-set log-messages-ref new-message-list))
	  nil)
	(catch Exception e
	  (. e printStackTrace)))
       nil))))
	
(defn ui-run-hook-list [frame-data keyword]
  (doseq [hook @(frame-data keyword)]
    (try
     (hook)
     (catch Exception e
       (. e printStackTrace)))))

(defstruct ui-frame-data :frame :win-data :log-pane :inspector-pane :log-messages-ref :log-length-ref
	   :file-watcher-system 
	   :close-hooks-ref
	   :hidden-hooks-ref
	   :shown-hooks-ref)

(defn cancel-filemod-time [filemod-timer]
  (when @filemod-timer
    (. @filemod-timer cancel)
    (dosync (ref-set filemod-timer nil))))

(defn create-timer-task [lmbda] 
  (proxy [TimerTask] []
    (run 
     []
     (lmbda))))

(defn check-and-update-log-window[log-messages-ref my-messages-ref log-window]
  (when-not (= @my-messages-ref @log-messages-ref)
    (dosync (ref-set my-messages-ref @log-messages-ref))
    (let [new-message-list @my-messages-ref
	  builder (StringBuilder.)]
      (. builder append "<html>")
      (doseq [msg (reverse new-message-list)]
	(. builder append msg)
	(. builder append "<br>\n"))
      (. builder append "</html>")
      (SwingUtilities/invokeLater 
       (fn []
	 (. log-window setText (. builder toString)))))))

(defn ui-create-app-frame 
  ([appName capabilities]
     (. (System/getProperties) setProperty "apple.laf.useScreenMenuBar" "true")
     (. (System/getProperties) setProperty "com.apple.mrj.application.apple.menu.about.name" appName)
     (let [frame (JFrame. appName)
	   { panel :gl-win gl-system-strs-ref :gl-system-strs-ref 
	    render-context-ref :render-context-ref :as win-data } ( ui-create-gl-win-data capabilities)
	   logger-ref (@render-context-ref :logger-ref)
	   bar (JMenuBar.)
	   menu (JMenu. "About")
	   file-mod-watcher-system (create-file-mod-watcher-system)
	   filemod-timer (ref nil)
	   [window-mgr log-label inspector] (init-tool-window-manager)]
       (.. frame getContentPane (setLayout (BorderLayout.)))
       (.. frame getContentPane (add window-mgr))
       (. (. window-mgr getContentManager) addContent "gl-panel" nil nil panel)
       (. bar add menu)
       (create-menu-item "OpenGL" (fn [ignored] (display-opengl-properties frame gl-system-strs-ref)) menu)
       (. frame setJMenuBar bar)
       (. frame setSize 1000 600)
       
       (SwingUtilities/invokeLater 
	(fn [] 
	  (. (. window-mgr getToolWindow "Log") setActive true)
	  (. (. window-mgr getToolWindow "Inspector") setActive true)
	  (. frame setVisible true)))
       (let [retval (struct-map ui-frame-data 
		      :frame frame
		      :win-data win-data
		      :log-pane log-label
		      :inspector-pane inspector
		      :log-messages-ref (ref nil) 
		      :log-length-ref (ref 1000)
		      :file-watcher-system file-mod-watcher-system
		      :close-hooks-ref (ref nil)
		      :hidden-hooks-ref (ref nil)
		      :shown-hooks-ref (ref nil))
	     window-listener (proxy [Object WindowListener] []
			       (windowActivated [-])
			       (windowClosed[-])
			       (windowClosing[-]
				(. frame setVisible false)
				(. frame dispose)
				(cancel-filemod-time filemod-timer)
				(ui-run-hook-list retval :close-hooks-ref)
				;destroy the render context
				(dosync (ref-set render-context-ref (create-render-context logger-ref)))
				)
			       (windowOpened [-])
			       (windowDeactivated [-])
			       (windowDeiconified [-])
			       (windowIconified [-]))
	     component-listener (proxy [Object ComponentListener] []
				  (componentHidden
				   [-] 
				   (cancel-filemod-time filemod-timer)
				   (ui-run-hook-list retval :hidden-hooks-ref))
				  (componentMoved[-] )
				  (componentResized[-] )
				  (componentShown
				   [-]
				   (try
				    (when (not @filemod-timer)
				      (dosync (ref-set filemod-timer (Timer.)))
				      (. @filemod-timer 
					 schedule  
					 (create-timer-task #(fs-mod-system-check-files file-mod-watcher-system ))
					 (Date.) 
					 (long 300))
				      (let [printed-messages-ref (ref nil)]
					(. @filemod-timer 
					   schedule  
					   (create-timer-task #(check-and-update-log-window (retval :log-messages-ref) printed-messages-ref log-label))
					   (Date.) 
					   (long 300))))
				    
				    (catch Exception e
				      (. e printStackTrace)))
				   (ui-run-hook-list retval :shown-hooks-ref)))]
	 (. frame addWindowListener window-listener)
	 (. frame addComponentListener component-listener)
	 (dosync (ref-set logger-ref (log-add-listener @logger-ref 
						       (fn [module type & args]
							 (ui-add-log-message retval module type args)))))
	 retval)))
  ([appName] (ui-create-app-frame appName nil)))

;:close-hooks-ref
;:hidden-hooks-ref
;:shown-hooks-ref
(defn ui-add-hook [frame-data keyword lmbda]
  (let [hooks-ref (frame-data keyword)]
    (when (and hooks-ref lmbda)
      (dosync (alter hooks-ref conj lmbda)))))

;add a todo item and tell the view to redraw itself
;drawable fn must take only one argument, that is the drawable
;this function calls invalidate on the glJPanel or does something
;to immediately schedule a render.
;If you want your function to keep rendering, then just have it keep calling
;this function with itself.
(defn ui-register-gl-drawable-todo-with-update [frame-data drawable-fn]
  (let [{ {gl-todo-list-ref :gl-todo-list-ref panel :gl-win } :win-data } frame-data]
    (add-gl-todo-item gl-todo-list-ref drawable-fn)
    (. panel display)))

;it is fine to set the drawable fn to nil.
(defn ui-set-gl-render-fn[frame-data drawable-fn]
  (let [{ {gl-render-fn-ref :gl-render-fn-ref panel :gl-win } :win-data } frame-data]
    (dosync (ref-set gl-render-fn-ref drawable-fn))))

;this runs a function on the render thread
;and returns the result, blocking the caller.
;if the result is an exception, it is wrapped and thrown
(defn ui-run-sync-delayed-gl-function[frame-data drawable-fn]
  (let [result-ref (ref nil)
	error-ref (ref nil)
	latch (CountDownLatch. 1)
	fn-wrapper (fn [drawable]
		     (try
		      (let [result (drawable-fn drawable)]
			(dosync (ref-set result-ref result)))
		      (catch Exception e
			(dosync (ref-set error-ref e)))
		      (finally
		       (. latch countDown))))]
    (ui-register-gl-drawable-todo-with-update frame-data fn-wrapper)
    (. latch await)
    (let [result @result-ref
	  error @error-ref]
      (if error
	(throw (Exception. error))
	result))))

(defn ui-run-gl-function-with-swap-and-update[frame-data drawable-fn]
  (let [fn-wrapper (fn [drawable]
		     (try
		      (drawable-fn drawable)
		      (catch Exception e)))]
    (ui-register-gl-drawable-todo-with-update frame-data fn-wrapper)))
  
(defn ui-get-rcgl-render-context-ref[frame-data]
  (let [{ { retval :render-context-ref } :win-data } frame-data]
    retval))

(defn ui-get-render-todo-list-ref [frame-data]
  (let [{ { retval :gl-todo-list-ref } :win-data } frame-data]
    retval))

(defn animator-render-update [drawable ui-animator]
  (let [default-render-fn-ref (ui-animator :default-render-fn-ref)
	render-fn-ref (ui-animator :render-fn-ref)
	render-fn @render-fn-ref
	default-render-fn @default-render-fn-ref]
    (if render-fn
      (render-fn drawable)
      (if default-render-fn
	(default-render-fn drawable)
	(let [gl (. drawable getGL)]
	  (. gl glClearColor 0.2 0.2 1.0 1.0)
	  (. gl glClear GL/GL_COLOR_BUFFER_BIT))))))

(defn ui-disable-fps-animator[frame-data]
  (let [{ { animator-ref :animator-ref :as win-data } :win-data } frame-data
	old-animator @animator-ref]
    (when old-animator
      (. old-animator stop))
    (dosync (ref-set animator-ref nil))))

(defn ui-set-fps-animator[frame-data inFPS]
  (let [{ { drawable :gl-win animator-ref :animator-ref } :win-data } frame-data
	animator (FPSAnimator. drawable inFPS true)
	old-animator @animator-ref]
    (ui-disable-fps-animator frame-data)
    (. animator start)
    (dosync (ref-set animator-ref animator))))

(defn ui-update-display [frame-data]
  (let [{ { panel :gl-win } :win-data } frame-data
	dims (. panel getSize)]
    (. panel repaint 0 0 (.width dims) (.height dims))))