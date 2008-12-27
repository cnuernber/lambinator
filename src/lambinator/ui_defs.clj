(in-ns 'lambinator.ui)

;File to define definitions that should be reset while editing
;the ui system.  The static definitions shouldn't be
;else you loose the definition of the system vars
;and various other static info.

(defonce gl_system_lookup_strs
  '("GL_EXTENSIONS" 
    "GL_VENDOR"
    "GL_RENDERER"
    "GL_VERSION"))

(defn get_supported_gl_extensions [gl_system_strs_ref]
  (re-seq (Pattern/compile "\\S+") (@gl_system_strs_ref "GL_EXTENSIONS")))

(defn get_sorted_htmlized_gl_extensions [gl_system_strs_ref]
  (let [bldr (StringBuilder.)]
    (. bldr append "<html>")
    (reduce 
     (fn [bldr str] (when (> (. bldr length) 6) (. bldr append "<BR>")) (. bldr append str) bldr) 
     bldr 
     (sort (get_supported_gl_extensions gl_system_strs_ref)))
    (. bldr append "</html>")
    (. bldr toString )))

(defn update_gl_system_strs [drawable gl_system_strs_ref] 
  (let [sys_strs @gl_system_strs_ref
	keys gl_system_lookup_strs
	gl (. drawable getGL)]
    (let [newStrs (reduce 
		   #(assoc %1 %2 
			   (. gl glGetString 
			      (get_static_field_value "javax.media.opengl.GL" %2))) 
		   sys_strs keys)]
      (dosync (ref-set gl_system_strs_ref newStrs)))))

(defn add_gl_todo_item [gl_todo_list_ref item]
  (dosync (ref-set gl_todo_list_ref (conj @gl_todo_list_ref item))))

(defn gl_init [drawable gl_system_strs_ref render_context_ref]
  (update_gl_system_strs drawable gl_system_strs_ref)
  (let [new_context (rcgl_resources_destroyed drawable @render_context_ref)]
    (dosync (ref-set render_context_ref (merge @render_context_ref new_context)))))

;Important that this gets wrapped in a try/catch
;if it doesn't, then you loose your ability to run the
;display method!
(defn run_gl_todo_item[drawable item render_exceptions_ref]
  (try
   (item drawable)
   (catch Exception e
     (dosync (ref-set render_exceptions_ref (conj @render_exceptions_ref e))))))

(defn gl_display [drawable gl_todo_list_ref render_exceptions_ref]
  (let [todoItems @gl_todo_list_ref]
    (dosync (ref-set gl_todo_list_ref nil))
    (doseq [item todoItems]
      (run_gl_todo_item drawable item render_exceptions_ref) todoItems)))

(defn gl_display_changed [drawable modelChanged devChanged])

(defn gl_reshape [drawable x y width height gl_dimensions_ref]
  (dosync (ref-set gl_dimensions_ref [x y width height])))

(defn create_gl_event_listener 
  [render_exceptions_ref 
   gl_dimensions_ref 
   gl_system_strs_ref 
   gl_todo_items_ref
   render_context_ref]
  (proxy [Object GLEventListener]
	  []
	(init [dble] (gl_init dble gl_system_strs_ref render_context_ref))

	(display [dble] (gl_display dble gl_todo_items_ref gl_system_strs_ref))

	(displayChanged [dble modeChanged devChanged]
			(gl_display_changed dble modeChanged devChanged ))

	(reshape [dble x y width height] (gl_reshape dble x y width height gl_dimensions_ref))))

;jlabels aren't selectable and copyable, which drives me fucking
;crazy.  Why can't you select and copy any text in a UI?
(defn create_label [text]
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

(defn create_scrollable_extensions_label [gl_system_strs_ref]
  (let [text (get_sorted_htmlized_gl_extensions gl_system_strs_ref)
	lbl (create_label text)
	pane (JScrollPane. lbl 
			   ScrollPaneConstants/VERTICAL_SCROLLBAR_AS_NEEDED
			   ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)]
    pane))

(defmacro add_with_constraints [item constraints parent & rest]
	`(do (sets! ~constraints ~@rest) (. ~parent add ~item ~constraints)))

(defn get_gl_system_property [name gl_system_strs_ref]
  (@gl_system_strs_ref name))

(defn display_opengl_properties [frame gl_system_strs_ref]
  ;open up something to display the system properties
  (let [dlg (JDialog. frame "Open GL Information")
	constraints (GridBagConstraints.) ]
    (. dlg setLayout (GridBagLayout.))
    (sets! constraints ipadx 1 ipady 1)
    (add_with_constraints (create_label "Version") constraints dlg
			  gridx 0
			  gridy 0
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (add_with_constraints (create_label (get_gl_system_property "GL_VERSION" gl_system_strs_ref)) constraints dlg
			  gridx 1
			  gridy 0
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (add_with_constraints (create_label "Vendor") constraints dlg
			  gridx 0
			  gridy 1
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (add_with_constraints (create_label (get_gl_system_property "GL_VENDOR" gl_system_strs_ref)) constraints dlg
			  gridx 1
			  gridy 1
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (add_with_constraints (create_label "Renderer") constraints dlg
			  gridx 0
			  gridy 2
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (add_with_constraints (create_label (get_gl_system_property "GL_RENDERER" gl_system_strs_ref)) constraints dlg
			  gridx 1
			  gridy 2
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (add_with_constraints (create_label "Extensions" ) constraints dlg
			  gridx 0
			  gridy 3
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (add_with_constraints (create_scrollable_extensions_label gl_system_strs_ref) constraints dlg
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
  
(defn create_action_listener [lmbda]
  (proxy [Object ActionListener]
      []
    (actionPerformed 
     [evt]
     (apply lmbda [evt]))))

(defn create_menu_item [title lmbda parent]
  (let [item (JMenuItem. title)]
    (. item addActionListener (create_action_listener lmbda))
    (. parent add item)
    item))