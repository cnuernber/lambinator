(ns lambinator.ui
  (:import (javax.media.opengl GLJPanel GLEventListener GL GLCapabilities)
	   (javax.swing JFrame JMenu JMenuBar JMenuItem UIManager JDialog JLabel
			JScrollPane ScrollPaneConstants JTextField JTextPane
			SwingUtilities JButton JPanel BoxLayout)
	   (javax.swing.text.html HTMLEditorKit)
	   (com.sun.opengl.util FPSAnimator)
	   (java.awt BorderLayout GridBagLayout GridBagConstraints Dimension Component)
	   (java.awt.event ActionListener WindowListener ComponentListener)
	   (java.util.regex Pattern)
	   (java.util.concurrent CountDownLatch)
	   (java.util Timer TimerTask Date)
	   (org.noos.xing.mydoggy ToolWindow ToolWindowAnchor ToolWindowManager)
	   (org.noos.xing.mydoggy.plaf MyDoggyToolWindowManager)) ;forcing async to sync
  (:use lambinator.util lambinator.rcgl lambinator.log
	clojure.contrib.seq-utils
	lambinator.fs lambinator.ui.gl lambinator.ui.util
	swank.swank
	(swank.core connection server)
	clojure.main 
	clojure.contrib.duck-streams ))


;creates all necessary ref's structs and returns a 
;you pass in a javax.media.opengl.GLCapabilities
;win data that contains a live glgpanel	
;[render-exceptions-ref gl-dimensions-ref 
; gl-system-strs-ref gl-todo-items-ref]

(defn- init-tool-window-manager
  "Initialize the tool manager for the mydoggy tool system"
  []
  (let [window-mgr (MyDoggyToolWindowManager.)
	log-pane (uiut-create-html-label "")
	newTool (JScrollPane. log-pane)
	inspector-panel (JPanel.)
	inspector-wrapper-panel (JPanel. )
	inspector-scroll (JScrollPane. inspector-panel
				       ScrollPaneConstants/VERTICAL_SCROLLBAR_AS_NEEDED
				       ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)]
    (. window-mgr registerToolWindow "Log" "Application Log" nil newTool ToolWindowAnchor/BOTTOM)
    (. window-mgr registerToolWindow "Inspector" "Inspector Palette" nil inspector-scroll ToolWindowAnchor/RIGHT)
    (doseq [window (. window-mgr getToolWindows)]
      (. window setAvailable true))
    [window-mgr log-pane inspector-panel]))

(defn ui-get-uigl-window-data
  "Get the uigl window data from the frame"
  [frame-data]
  (frame-data :win-data))

(defn ui-add-log-message
  "Add a new log message to the logging window.  This function puts a new function
on the CPU thread that appends the log message to the list.  Each line of the message
is prefixed with the module and type where the data came from.  There is another thread
that listens for changes to the log messages list every 1/3 second or so that creates a
new string from the messages.  Finally it calls SwingUtils/invokeLater to replace
the log window text with new text"
  [frame-data module type & args]
  (let [log-window (frame-data :log-pane)
	agent (agent {})]
    (send agent 
     (fn [_]
       (try
	(let [log-messages-ref (frame-data :log-messages-ref)
	      args-str-lines (reverse (map (fn [line]
					     (util-stringify module " " (name type) ": " line))
					   (util-split-on-newline (apply util-stringify (flatten args)))))
	      new-message-list (take @(frame-data :log-length-ref) (concat args-str-lines @log-messages-ref))]
	  (dosync (ref-set log-messages-ref new-message-list))
	  nil)
	(catch Exception e
	  (. e printStackTrace)))
       nil))))
	
(defn- run-hook-list [frame-data keyword]
  (doseq [hook (reverse @(frame-data keyword))]
    (try
     (hook)
     (catch Exception e
       (. e printStackTrace)))))

(defstruct ui-frame-data 
  :frame  ;JFrame 
  :win-data ;gl-window-data
  :log-pane ;JPanel
  :inspector-pane ;JPanel
  :log-messages-ref ;(ref nil)
  :log-length-ref ;(ref 1000)
  :file-watcher-system ;(lambinator.fs.file-watcher-system)
  :close-hooks-ref ;(ref nil)
  :hidden-hooks-ref ;(ref nil)
  :shown-hooks-ref) ;(ref nil)

(defn- cancel-filemod-time [filemod-timer]
  (when @filemod-timer
    (. @filemod-timer cancel)
    (dosync (ref-set filemod-timer nil))))

(defn- create-timer-task [lmbda] 
  (proxy [TimerTask] []
    (run 
     []
     (lmbda))))

(defn- check-and-update-log-window[log-messages-ref my-messages-ref log-window]
  (when-not (= @my-messages-ref @log-messages-ref)
    (dosync (ref-set my-messages-ref @log-messages-ref))
    (let [new-message-list @my-messages-ref
	  text (util-htmlize-string-sequence (reverse new-message-list))]
      (SwingUtilities/invokeLater 
       (fn []
	 (. log-window setText text))))))

(defn- display-opengl-properties 
  "Pop up a dialog and display the opengl system properties" 
  [gl-window-data jframe]
  (let [dialog (JDialog. jframe)]
    (uigl-setup-properties-panel dialog gl-window-data)
    (doto dialog
      .pack
      (.setSize (.. dialog getPreferredSize width) 300)
      .show )))

(defn ui-get-gl-window-data [frame-data]
  (frame-data :win-data))

;Used so items can find themselves from the repl
(def ui-app-frames (ref nil))


(defn ui-start-swank-server
  "Start a swank server.  I think this prints out
a port number but I am not certain"
  [frame-data logger-ref]
  (with-bindings 
    (let [fname (.getCanonicalPath (fs-get-temp-file "swank"))]
      (start-server fname)
      (log-message @logger-ref "ui" :info "Started swank server: " (slurp fname)))))
		    
(defn ui-create-app-frame 
  "Create the application frame.  A frame consists of a central GLJPanel along with
a log tool window and an inspector tool window
appName - Name of the application.
capabilities - a GLCapabilities object or nil"
  ([appName capabilities]
     (. (System/getProperties) setProperty "apple.laf.useScreenMenuBar" "true")
     (. (System/getProperties) setProperty "com.apple.mrj.application.apple.menu.about.name" appName)
     (let [frame (JFrame. appName)
	   logger-ref (ref (create-log-data))
	   { panel :gl-win gl-system-strs-ref :gl-system-strs-ref 
	    render-context-ref :render-context-ref :as win-data } (uigl-create-gl-window-data capabilities logger-ref)
	   bar (JMenuBar.)
	   menu (JMenu. "About")
	   file-mod-watcher-system (create-file-mod-watcher-system)
	   filemod-timer (ref nil)
	   [window-mgr log-label inspector] (init-tool-window-manager)]
       (.. frame getContentPane (setLayout (BorderLayout.)))
       (.. frame getContentPane (add window-mgr))
       (. (. window-mgr getContentManager) addContent "gl-panel" nil nil panel)
       (. bar add menu)
       (uiut-create-menu-item "OpenGL" 
			      (fn [_] (display-opengl-properties win-data frame)) 
			      menu)
       (. frame setJMenuBar bar)
       (. frame setSize 1000 600)
       
       (SwingUtilities/invokeLater 
	(fn [] 
	  (. (. window-mgr getToolWindow "Log") setActive true)
	  (. (. window-mgr getToolWindow "Inspector") setActive true)))
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
			       (windowActivated [_])
			       (windowClosed[_])
			       (windowClosing[_]
				(. frame setVisible false)
				(. frame dispose)
				(cancel-filemod-time filemod-timer)
				(run-hook-list retval :close-hooks-ref)
				;destroy the render context
				(dosync (ref-set render-context-ref (create-render-context logger-ref)))
				)
			       (windowOpened [_])
			       (windowDeactivated [_])
			       (windowDeiconified [_])
			       (windowIconified [_]))
	     component-listener (proxy [Object ComponentListener] []
				  (componentHidden
				   [_] 
				   (cancel-filemod-time filemod-timer)
				   (run-hook-list retval :hidden-hooks-ref))
				  (componentMoved[_] )
				  (componentResized[_] )
				  (componentShown
				   [_]
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
					   (create-timer-task #(check-and-update-log-window 
								(retval :log-messages-ref) 
								printed-messages-ref 
								log-label))
					   (Date.) 
					   (long 300))))
				    
				    (catch Exception e
				      (. e printStackTrace)))
				   (run-hook-list retval :shown-hooks-ref)))]
	 (. frame addWindowListener window-listener)
	 (. frame addComponentListener component-listener)
	 (dosync (ref-set logger-ref (log-add-listener @logger-ref 
						       (fn [module type & args]
							 (ui-add-log-message 
							  retval 
							  module 
							  type 
							  args)))))
	 (dosync (alter ui-app-frames conj retval))
	 (uiut-create-menu-item "Start Swank" (fn [_] (ui-start-swank-server retval logger-ref)) menu)
	 retval)))
  ([appName] (ui-create-app-frame appName nil)))

(defn- get-menu-bar[frame]
  (let [jframe (frame :frame)]
    (.getJMenuBar jframe)))
  

(defn ui-get-top-level-menu-item
  "Return a top level menu item of a particular name"
  [frame name]
  (let [mbar (get-menu-bar frame)
	name-items (map (fn [index]
			  (let [comp (.getComponent mbar index)]
			    [(.getText comp ) comp]))
			(range (.getComponentCount mbar)))
	existing (first (filter (fn [[item-name comp]] (= item-name name)) name-items))]
    (when existing
      (second existing))))

(defn ui-remove-top-level-menu-item
  "Remove a top level menu item of a particular name"
  [frame name]
  (let [mbar (get-menu-bar frame)
	item (ui-get-top-level-menu-item frame name)]
    (when item
      (.remove mbar item))))

(defn ui-create-top-level-menu-item
  "Return a top level menu item of a particular name"
  [frame name]
  (let [mbar (get-menu-bar frame)
	new-item (JMenu. name)]
    (ui-remove-top-level-menu-item frame name)
    (.add mbar new-item)))

(defn ui-get-inspector-panel [frame]
  (frame :inspector-pane))

;:close-hooks-ref
;:hidden-hooks-ref
;:shown-hooks-ref
(defn ui-add-hook 
  "Available hooks are:
close-hooks-ref - called when the window is closed
hidden-hooks-ref - called with the window is hidden
shown-hooks-ref - called when the window is shown (including opened)" 
  [frame-data keyword lmbda]
  (let [hooks-ref (frame-data keyword)]
    (when (and hooks-ref lmbda)
      (dosync (alter hooks-ref conj lmbda)))))