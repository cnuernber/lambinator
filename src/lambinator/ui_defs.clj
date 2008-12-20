(in-ns 'lambinator.ui)
(import '(javax.media.opengl GLJPanel GLEventListener GL))
(import '(javax.swing JFrame JMenu JMenuBar JMenuItem UIManager JDialog JLabel
		      JScrollPane ScrollPaneConstants JTextField JTextPane))
(import	'(java.awt BorderLayout GridBagLayout GridBagConstraints Dimension))
(import '(java.awt.event ActionListener))
(import	'(java.util.regex Pattern))
(use 'lambinator.util)

;File to define definitions that should be reset while editing
;the ui system.  The static definitions shouldn't be
;else you loose the definition of the system vars
;and various other static info.

;this file should be mainly functions


(defn get_supported_gl_extensions []
  (re-seq (Pattern/compile "\\S+") (@gl_system_strs "GL_EXTENSIONS")))

(defn get_sorted_concatenated_gl_extensions []
     (. (reduce 
	 (fn [bldr str] (when (. bldr length) (. bldr append "\n")) (. bldr append str) bldr) 
	 (StringBuilder.) 
	 (sort (get_supported_gl_extensions)))
	toString ))

(defn get_sorted_htmlized_gl_extensions []
  (let [bldr (StringBuilder.)]
    (. bldr append "<html>")
    (reduce 
     (fn [bldr str] (when (> (. bldr length) 6) (. bldr append "<BR>")) (. bldr append str) bldr) 
     bldr 
     (sort (get_supported_gl_extensions)))
    (. bldr append "</html>")
    (. bldr toString )))
    

(defn update_gl_system_strs [drawable]
  (let [sys_strs @gl_system_strs
	keys gl_system_lookup_strs
	gl (. drawable getGL)]
    (let [newStrs (reduce 
		   #(assoc %1 %2 
			   (. gl glGetString 
			      (get_static_field_value "javax.media.opengl.GL" %2))) 
		   sys_strs keys)]
      (dosync (ref-set gl_system_strs newStrs)))))


;function callable with one argument; the gl interface
(defn add_gl_todo_item [item]
  (dosync (ref-set gl_todo_list (cons item @gl_todo_list))))

(defn gl_init [drawable]
  (update_gl_system_strs drawable))

(defn gl_display [drawable]
  (let [todoItems @gl_todo_list]
    (dosync (ref-set gl_todo_list nil))
    (let [tempItem (map #(apply %1 (list drawable)) todoItems)]
      (dorun tempItem))))

(defn gl_display_changed [drawable modelChanged devChanged]
  (println "gl display changed"))

(defn gl_reshape [drawable x y width height])

(defn create_gl_event_listener []
  (proxy [Object GLEventListener]
	  []
	(display [dble] (gl_display dble))
	(displayChanged [dble modeChanged devChanged]
			(gl_display_changed dble modeChanged devChanged ))
	(init [dble] (gl_init dble))
	(reshape [dble x y width height] (gl_reshape dble x y width height))))

;jlabels aren't selectable and copyable, which drives me fucking
;crazy.  Why can't you select and copy any text in a UI?
(defn create_label [text]
  (let [field (JTextPane.)]
    (doto field
      (.setContentType "text/html")
      (.setText text)
      (.setBorder nil)
      (.setOpaque false)
      (.setEditable false))
    field))

(defn create_scrollable_extensions_label []
  (let [text (get_sorted_htmlized_gl_extensions)
	lbl (create_label text)
	pane (JScrollPane. lbl 
			   ScrollPaneConstants/VERTICAL_SCROLLBAR_AS_NEEDED
			   ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)]
    pane))

(defmacro add_with_constraints [item constraints parent & rest]
	`(do (sets! ~constraints ~@rest) (. ~parent add ~item ~constraints)))

(defn get_gl_system_property [name]
  (@gl_system_strs name))

(defn display_opengl_properties [frame]
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
    (add_with_constraints (create_label (get_gl_system_property "GL_VERSION")) constraints dlg
			  gridx 1
			  gridy 0
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (add_with_constraints (create_label "Vendor") constraints dlg
			  gridx 0
			  gridy 1
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (add_with_constraints (create_label (get_gl_system_property "GL_VENDOR")) constraints dlg
			  gridx 1
			  gridy 1
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (add_with_constraints (create_label "Renderer") constraints dlg
			  gridx 0
			  gridy 2
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (add_with_constraints (create_label (get_gl_system_property "GL_RENDERER")) constraints dlg
			  gridx 1
			  gridy 2
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/HORIZONTAL)
    (add_with_constraints (create_label "Extensions") constraints dlg
			  gridx 0
			  gridy 3
			  anchor GridBagConstraints/NORTHWEST
			  fill GridBagConstraints/NONE)
    (add_with_constraints (create_scrollable_extensions_label) constraints dlg
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
     (apply lmbda (list evt)))))

(defn create_menu_item [title lmbda parent]
  (let [item (JMenuItem. title)]
    (. item addActionListener (create_action_listener lmbda))
    (. parent add item)
    item))
	


(defn create_app_frame []
  (let [appName "Lambit up"]
    (. (System/getProperties) setProperty "apple.laf.useScreenMenuBar" "true")
    (. (System/getProperties) 
       setProperty "com.apple.mrj.application.apple.menu.about.name" appName)
    (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
    (let [frame (JFrame. appName)
	  panel (GLJPanel. )
	  bar (JMenuBar.)
	  menu (JMenu. "About")]
      (.. frame getContentPane (setLayout (BorderLayout.)))
      (.. frame getContentPane (add panel))
      (. panel addGLEventListener (create_gl_event_listener))
      (. bar add menu)
      (create_menu_item "OpenGL" (fn [ignored] (display_opengl_properties frame)) menu)
      (. frame setJMenuBar bar)
      (. frame show)
      (def main_frame frame)
      frame)))