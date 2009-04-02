(ns lambinator.ui.gl
  (:import (javax.media.opengl GLJPanel GLEventListener GL GLCapabilities GLCanvas)
	   (java.awt GridBagLayout GridBagConstraints)
	   (com.sun.opengl.util FPSAnimator)
	   (java.util.concurrent CountDownLatch)
	   (javax.swing SwingUtilities JScrollPane ScrollPaneConstants)
	   (java.awt Dimension)
	   	   )
  (:use lambinator.rcgl lambinator.rc lambinator.util lambinator.log lambinator.ui.util
	lambinator.rcgl.fbo ))


;Items looked up using glGetString
;These are displayed in the order they are given...
(defonce gl-system-lookup-strs
  [["GL_VERSION" "Version" :single]
   ["GL_VENDOR" "Vendor" :single]
   ["GL_RENDERER" "Renderer" :single]
   ["GL_SHADING_LANGUAGE_VERSION" "Glsl Version" :single]
   ["GL_EXTENSIONS" "Extensions" :multi]])

(defstruct gl-window-data-struct
  :gl-win ;GLJpanel
  :gl-system-strs-ref  ;system data
  :gl-todo-list-ref    ;todo functions, run once then cleared
  :gl-dimensions-ref   ;currently known dimensions
  :render-context-ref  ;reference to render context
  :gl-render-fn-ref    ;render function, run every display
  :animator-ref        ;ref to the animator item for this window
  :timer-ref           ;ref to the timer used for watching the todo list
  )


(defn uigl-get-supported-gl-extensions 
  "Get the list of supported gl extensions."
  [gl-window-data]
  (util-split-on-whitespace (@(gl-window-data :gl-system-strs-ref) "GL_EXTENSIONS")))

(defn uigl-get-sorted-htmlized-gl-extensions 
  "Sort the gl extensions by name, then return a string beginning with <html>, 
ending with </html>, with each string but the last with a <br> appended."
  [gl-window-data]
  (let [bldr (StringBuilder.)]
    (. bldr append "<html>")
    (reduce 
     (fn [bldr str] (when (> (. bldr length) 6) (. bldr append "<BR>")) (. bldr append str) bldr) 
     bldr 
     (sort (uigl-get-supported-gl-extensions gl-window-data)))
    (. bldr append "</html>")
    (. bldr toString )))


(defn- update-gl-system-strs [drawable gl-system-strs-ref] 
  (let [sys-strs @gl-system-strs-ref
	keys (map #(% 0) gl-system-lookup-strs)
	gl (. drawable getGL)]
    (let [newStrs (reduce 
		   #(assoc %1 %2
			   (. gl glGetString 
			      (util-get-static-field-value "javax.media.opengl.GL" %2))) 
		   sys-strs keys)]
      (dosync (ref-set gl-system-strs-ref newStrs)))))

;You have the choice of three different ways you can add a todo item.
;You can add one and let the system decide when to refresh.
;You can add one and force an immediate refresh, and finally you
;can add one, force a refresh, and wait for the result.
(defn uigl-add-gl-todo-item 
  "Add a gl item that will get run before the gl render function next time
the window needs to display"
  [gl-window-data item]
  (dosync (alter (gl-window-data :gl-todo-list-ref) conj item)))

(defn uigl-add-gl-todo-item-with-update 
  "Add a gl todo item and immediately call update"
  [gl-window-data drawable-fn]
  (let [{ panel :gl-win }  gl-window-data]
    (uigl-add-gl-todo-item gl-window-data drawable-fn)
    (SwingUtilities/invokeLater (fn [] (. panel display)))))


;this runs a function on the render thread
;and returns the result, blocking the caller.
;if the result is an exception, it is wrapped and thrown
(defn uigl-run-gl-todo-item
  "Run a gl todo item.  This requires waiting for the result.
This function returns the return value of the drawable-fn"
  [gl-window-data drawable-fn]
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
    (uigl-add-gl-todo-item-with-update gl-window-data fn-wrapper)
    (. latch await)
    (let [result @result-ref
	  error @error-ref]
      (if error
	(throw (Exception. error))
	result))))

(defn uigl-set-render-fn
  "Set the render function.  This will get called every render, after the
todo items have finished.  The function is expected to take one argument,
a GLAutoDrawable.  You can get the gl interface from the drawable by calling
getGL"
  [gl-window-data drawable-fn]
  (dosync (ref-set (gl-window-data :gl-render-fn-ref) drawable-fn)))

(defn uigl-get-logger-ref
  "Return the logger currently used for this gl window"
  [gl-window-data]
  (@(gl-window-data :render-context-ref) :logger-ref))

(defn- uigl-log-message[gl-window-data type & args]
  (let [logger @(uigl-get-logger-ref gl-window-data)]
    (when logger
      (log-message logger "ui.gl" type args))))

(defn- gl-init 
  "Called upon initialization of the gl system"
  [drawable gl-window-data]
  (let [render-context-ref (gl-window-data :render-context-ref)]
    (rcglf-test-create-textured-fbo drawable 256 256)
    (rcglf-print-buffer-status drawable)
    (uigl-log-message gl-window-data :info "gl initialized, rebuilding render context")
    (try
     (update-gl-system-strs drawable (gl-window-data :gl-system-strs-ref))
     (let [new-context (rcgl-resources-destroyed drawable @render-context-ref)]
       (dosync (ref-set render-context-ref (merge @render-context-ref new-context))))
     (catch Exception e
       (println "Error duing gl-init: " (. e printStackTrace))))))


;Important that this gets wrapped in a try/catch
;if it doesn't, then you loose your ability to run the
;display method!
(defn- run-gl-drawable[drawable item]
  (try
   (item drawable)
   (catch Exception e
     (. e printStackTrace))))

(defn- gl-display 
  [drawable gl-window-data]
  (let [todo-items-ref (gl-window-data :gl-todo-list-ref)
	todo-items (reverse @todo-items-ref)
	render-fn @(gl-window-data :gl-render-fn-ref)
	gl (.getGL drawable)
	int-array (make-array Integer/TYPE 1)]
    (dosync (ref-set todo-items-ref nil))
    (.glPushAttrib gl GL/GL_ALL_ATTRIB_BITS)
    (.glPushClientAttrib gl GL/GL_ALL_CLIENT_ATTRIB_BITS)
    (. gl glGetIntegerv GL/GL_FRAMEBUFFER_BINDING_EXT int-array 0)
    (doseq [item todo-items]
      (run-gl-drawable drawable item))
    ;Anything that allocates an FBO may set the wrong base fbo up when finished
    (. gl GL/glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT (aget int-array 0))
    (if render-fn
      (run-gl-drawable drawable render-fn)
      (do
	(. gl glClearColor 0.05 0.05 0.1 1.0)
	(. gl glClear GL/GL_COLOR_BUFFER_BIT)))
    (.glPopAttrib gl)
    (.glPopClientAttrib gl)))
    

(defn- gl-display-changed 
  [drawable modelChanged devChanged gl-window-data]
  (uigl-log-message gl-window-data :info "gl display changed"))

(defn- gl-reshape 
  [drawable x y width height gl-window-data]
  (uigl-log-message gl-window-data :info "gl reshape")
  (dosync (ref-set (gl-window-data :gl-dimensions-ref) [x y width height])))


(defn- create-gl-event-listener 
  [gl-window-data]
  (proxy [Object GLEventListener]
	  []
	(init 
	 [dble] 
	 (gl-init dble gl-window-data))

	(display 
	 [dble] 
	 (gl-display dble gl-window-data))

	(displayChanged 
	 [dble modeChanged devChanged]
	 (gl-display-changed dble modeChanged devChanged gl-window-data))

	(reshape 
	 [dble x y width height] 
	 (gl-reshape dble x y width height gl-window-data))))

(defn uigl-create-gl-window-data
  "Create a GLJPanel.  
glcapabilities - gl capabilities object or nil
logger_ref reference to a logger or nil"
  [glcapabilities logger_ref]
  (let [panel (GLJPanel. glcapabilities)
	window-data (struct-map gl-window-data-struct
		      :gl-win panel
		      :gl-system-strs-ref (ref {})
		      :gl-todo-list-ref (ref nil)
		      :gl-dimensions-ref (ref [0 0 0 0])
		      :render-context-ref (ref (create-render-context logger_ref))
		      :gl-render-fn-ref (ref nil)
		      :animator-ref (ref nil)
		      :timer-ref (ref nil))	
	listener (create-gl-event-listener 
		  window-data)]
    (. panel addGLEventListener listener)
    (.show panel)
    window-data))

(defn uigl-get-gl-system-property [name gl-window-data]
  (@(gl-window-data :gl-system-strs-ref) name))


(defn- create-scrollable-extensions-label [htmlized-extensions]
  (let [text htmlized-extensions
	lbl (uiut-create-html-label text)
	pane (JScrollPane. lbl 
			   ScrollPaneConstants/VERTICAL_SCROLLBAR_AS_NEEDED
			   ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)]
    (.show lbl)
    (.show pane)
    pane))

(defmulti uigl-create-control (fn [type data] type))
(defmethod uigl-create-control :default [_ data] (uiut-create-html-label data))
(defmethod uigl-create-control :multi [_ data] 
  (let [split-data (util-split-on-whitespace  data)
	htmlized-data (util-htmlize-string-sequence split-data)]
    (create-scrollable-extensions-label htmlized-data)))

(defn uigl-setup-properties-panel
  "Given a panel (or container of some sort) setup the known
gl system variables on the panel"
  [panel gl-window-data]
  (let [system-strs @(gl-window-data :gl-system-strs-ref)
	controls (map (fn [[key display-name type]]
			(let [new-label (uiut-create-html-label display-name)
			      preferred-size (.getPreferredSize new-label)]
			  (.show new-label)
			  (.setMinimumSize new-label (Dimension. 
						      (.width preferred-size) 
						      (.. new-label getMinimumSize height)))
			  [new-label
			   (uigl-create-control type (system-strs key))]))
		      gl-system-lookup-strs )
	controls_nil (lazy-cat controls (list nil))
	constraints (GridBagConstraints. )
	controls-index-next (map vector controls (iterate inc 0) (rest controls_nil))]
    (. panel setLayout (GridBagLayout. ))
    (sets! constraints ipadx 1 ipady 1)
    (doseq [[[name-control data-control] index next] controls-index-next]
      (let [[fill-right wy] (if next
				   [GridBagConstraints/HORIZONTAL 0.0]
				   [GridBagConstraints/BOTH 1.0])]
	(util-add-with-constraints name-control constraints panel
				   gridx 0
				   gridy index
				   anchor GridBagConstraints/NORTHWEST
				   fill GridBagConstraints/NONE
				   weightx 0.0
				   weighty wy )
	(util-add-with-constraints data-control constraints panel
				   gridx 1
				   gridy index
				   anchor GridBagConstraints/NORTHWEST
				   fill fill-right
				   weightx 1.0
				   weighty wy)))))

(defn uigl-get-render-context-ref 
  "get the render context for this gl window object"
  [gl-window-data]
  (gl-window-data :render-context-ref))

(defn uigl-get-gl-todo-list-ref
  "get the gl todo list for this gl window object"
  [gl-window-data]
  (gl-window-data :gl-todo-list-ref))

(defn uigl-with-render-context-ref-and-todo-list-ref
  "Run the given function with a render context ref and a
todo list ref.
lambda - function taking two arguments, a render-context-ref and a todo-list-ref"
  [{ rc-ref :render-context-ref todo-list-ref :gl-todo-list-ref } lambda]
  (lambda rc-ref todo-list-ref))

(defn uigl-disable-fps-animator
  "Disable the current fps animator for this gl panel
gl-window-data - the window data structure"
  [{ animator-ref :animator-ref }]
  (let [old-animator @animator-ref]
    (when old-animator
      (. old-animator stop))
    (dosync (ref-set animator-ref nil))))

(defn uigl-set-fps-animator
  "Set this gl panel to update a given fps
gl-window-data - the window data structure
inFPS - the new fps to run things at"
  [{ drawable :gl-win animator-ref :animator-ref :as gl-window-data }  inFPS]
  (let [animator (FPSAnimator. drawable inFPS true)
	old-animator @animator-ref]
    (uigl-disable-fps-animator gl-window-data)
    (. animator start)
    (dosync (ref-set animator-ref animator))))

(defn uigl-repaint [gl-window-data]
  (let [{drawable :gl-win} gl-window-data]
    (.revalidate drawable)
    (.repaint drawable)))

(defn uigl-load-glsl
  "Load a glslv file, used from the repl"
  [gl-window-data fname]
  (uigl-with-render-context-ref-and-todo-list-ref 
   gl-window-data
   (fn [rc rl]
     (rcgl-load-shader rc rl fname))))

(defn uigl-todo-watcher-enabled? 
  [gl-window-data]
  (not (nil? @(gl-window-data :timer-ref))))

(defn uigl-disable-todo-watcher
  [gl-window-data]
  (when (uigl-todo-watcher-enabled? gl-window-data)
    (let [timer-ref (gl-window-data :timer-ref)]
      (util-cancel-timer @timer-ref)
      (dosync (ref-set timer-ref nil)))))

(defn uigl-enable-todo-watcher
  "Start up a timer that watches the todo list and
calls repaint any time the list has items in it.  Useful
for systems where you want to repaint when things are loaded"
  [gl-window-data]
  (when-not (uigl-todo-watcher-enabled? gl-window-data)
    (let [new-timer (util-create-timer)
	  todo-ref (gl-window-data :gl-todo-list-ref)
	  lambda (fn []
		   (when-not (== 0 (count @todo-ref))
		     (uigl-repaint gl-window-data)))]
      (util-add-timer-task new-timer lambda 300)
      (dosync (ref-set (gl-window-data :timer-ref) new-timer)))))

(defn uigl-add-mouse-input-listener
  "Listener created via uiut-create-mouse-input-listener"
  [gl-window-data input-listener]
  (let [win (gl-window-data :gl-win)]
    (.addMouseMotionListener win input-listener)
    (.addMouseListener win input-listener)))

(defn uigl-remove-mouse-input-listener
  "Remove a listener created before"
  [gl-window-data input-listener]
    (let [win (gl-window-data :gl-win)]
    (.removeMouseMotionListener win input-listener)
    (.removeMouseListener win input-listener)))