(in-ns 'lambinator.ui)

;File to define definitions that should be reset while editing
;the ui system.  The static definitions shouldn't be
;else you loose the definition of the system vars
;and various other static info.

(defonce gl-system-lookup-strs
  '("GL_EXTENSIONS" 
    "GL_VENDOR"
    "GL_RENDERER"
    "GL_SHADING_LANGUAGE_VERSION"
    "GL_VERSION"))

(defn get-supported-gl-extensions [gl-system-strs-ref]
  (re-seq (Pattern/compile "\\S+") (@gl-system-strs-ref "GL_EXTENSIONS")))

(defn get-sorted-htmlized-gl-extensions [gl-system-strs-ref]
  (let [bldr (StringBuilder.)]
    (. bldr append "<html>")
    (reduce 
     (fn [bldr str] (when (> (. bldr length) 6) (. bldr append "<BR>")) (. bldr append str) bldr) 
     bldr 
     (sort (get-supported-gl-extensions gl-system-strs-ref)))
    (. bldr append "</html>")
    (. bldr toString )))

(defn update-gl-system-strs [drawable gl-system-strs-ref] 
  (let [sys-strs @gl-system-strs-ref
	keys gl-system-lookup-strs
	gl (. drawable getGL)]
    (let [newStrs (reduce 
		   #(assoc %1 %2 
			   (. gl glGetString 
			      (util-get-static-field-value "javax.media.opengl.GL" %2))) 
		   sys-strs keys)]
      (dosync (ref-set gl-system-strs-ref newStrs)))))

(defn add-gl-todo-item [gl-todo-list-ref item]
  (dosync (ref-set gl-todo-list-ref (conj @gl-todo-list-ref item))))

(defn gl-init [drawable gl-system-strs-ref render-context-ref]
  (try
   (update-gl-system-strs drawable gl-system-strs-ref)
   (let [new-context (rcgl-resources-destroyed drawable @render-context-ref)]
     (dosync (ref-set render-context-ref (merge @render-context-ref new-context))))
   (catch Exception e
     (println "Error duing gl-init: " (. e printStackTrace)))))

;Important that this gets wrapped in a try/catch
;if it doesn't, then you loose your ability to run the
;display method!
(defn run-gl-todo-item[drawable item render-exceptions-ref]
  (try
   (item drawable)
   (catch Exception e
     (. e printStackTrace))))

(defn gl-display [drawable gl-todo-list-ref render-exceptions-ref gl-render-fn-ref]
  (let [todoItems (reverse @gl-todo-list-ref)
	render-fn @gl-render-fn-ref]
    (dosync (ref-set gl-todo-list-ref nil))
    (doseq [item todoItems]
      (run-gl-todo-item drawable item render-exceptions-ref))
    (if render-fn
      (run-gl-todo-item drawable render-fn render-exceptions-ref)
      (let [gl (. drawable getGL)]
	(. gl glClearColor 0.05 0.05 0.1 1.0)
	(. gl glClear GL/GL_COLOR_BUFFER_BIT)))))

(defn gl-display-changed [drawable modelChanged devChanged])

(defn gl-reshape [drawable x y width height gl-dimensions-ref]
  (dosync (ref-set gl-dimensions-ref [x y width height])))

(defn create-gl-event-listener 
  [render-exceptions-ref 
   gl-dimensions-ref 
   gl-system-strs-ref 
   gl-todo-items-ref
   render-context-ref
   gl-render-fn-ref]
  (proxy [Object GLEventListener]
	  []
	(init [dble] (gl-init dble gl-system-strs-ref render-context-ref))

	(display [dble] (gl-display dble gl-todo-items-ref render-exceptions-ref gl-render-fn-ref))

	(displayChanged [dble modeChanged devChanged]
			(gl-display-changed dble modeChanged devChanged ))

	(reshape [dble x y width height] (gl-reshape dble x y width height gl-dimensions-ref))))

;jlabels aren't selectable and copyable, which drives me fucking
;crazy.  Why can't you select and copy any text in a UI?
(defn create-label [text]
  (let [field (JTextPane.)]
    (try 
     (. field setEditorKit (HTMLEditorKit. ))
     (catch NullPointerException e)) ;don't care
    (doto field
      (.setText text)
      (.setBorder nil)
      (.setOpaque false)
      (.setEditable false))
    field))

(defn create-scrollable-extensions-label [gl-system-strs-ref]
  (let [text (get-sorted-htmlized-gl-extensions gl-system-strs-ref)
	lbl (create-label text)
	pane (JScrollPane. lbl 
			   ScrollPaneConstants/VERTICAL_SCROLLBAR_AS_NEEDED
			   ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)]
    pane))

(defn get-gl-system-property [name gl-system-strs-ref]
  (@gl-system-strs-ref name))

(defn display-opengl-properties [frame gl-system-strs-ref]
  ;open up something to display the system properties
  (let [dlg (JDialog. frame "Open GL Information")
	constraints (GridBagConstraints.) ]
    (. dlg setLayout (GridBagLayout.))
    (sets! constraints ipadx 1 ipady 1)
    (util-add-with-constraints (create-label "Version") constraints dlg
			  gridx 0
			  gridy 0
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (util-add-with-constraints (create-label (get-gl-system-property "GL_VERSION" gl-system-strs-ref)) constraints dlg
			  gridx 1
			  gridy 0
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (util-add-with-constraints (create-label "Vendor") constraints dlg
			  gridx 0
			  gridy 1
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (util-add-with-constraints (create-label (get-gl-system-property "GL_VENDOR" gl-system-strs-ref)) constraints dlg
			  gridx 1
			  gridy 1
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (util-add-with-constraints (create-label "Renderer") constraints dlg
			  gridx 0
			  gridy 2
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (util-add-with-constraints (create-label (get-gl-system-property "GL_RENDERER" gl-system-strs-ref)) constraints dlg
			  gridx 1
			  gridy 2
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (util-add-with-constraints (create-label "Extensions" ) constraints dlg
			  gridx 0
			  gridy 3
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (util-add-with-constraints (create-scrollable-extensions-label gl-system-strs-ref) constraints dlg
			  gridx 1
			  gridy 3
			  weightx 1.0
			  weighty 1.0
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/BOTH)
    (doto dlg
      (.pack) ;now we have a preferred width
      (.setSize (.. dlg getPreferredSize width) 300)
      (.show))))
  
(defn create-action-listener [lmbda]
  (proxy [Object ActionListener]
      []
    (actionPerformed 
     [evt]
     (apply lmbda [evt]))))

(defn create-menu-item [title lmbda parent]
  (let [item (JMenuItem. title)]
    (. item addActionListener (create-action-listener lmbda))
    (. parent add item)
    item))