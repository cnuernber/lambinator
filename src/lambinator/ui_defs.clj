(in-ns 'lambinator.ui)
(import '(javax.media.opengl GLJPanel GLEventListener GL))
(import '(javax.swing JFrame JMenu JMenuBar JMenuItem UIManager JDialog))
(import	'(java.awt BorderLayout))
(import '(java.awt.event ActionListener))
(import	'(java.util.regex Pattern))
(require 'lambinator.util)

;File to define definitions that should be reset while editing
;the ui system.  The static definitions shouldn't be
;else you loose the definition of the system vars
;and various other static info.

;this file should be mainly functions


(defn get_supported_gl_extensions []
  (re-seq (Pattern/compile "\\S+") (@gl_system_strs "GL_EXTENSIONS")))

(defn update_gl_system_strs [drawable]
  (let [sys_strs @gl_system_strs
	keys gl_system_lookup_strs
	gl (. drawable getGL)]
    (let [newStrs (reduce 
		   #(assoc %1 %2 
			   (. gl glGetString 
			      (lambinator.util/get_static_field_value "javax.media.opengl.GL" %2))) 
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

(defn display_opengl_properties [frame evt_ignored]
  ;open up something to display the system properties
  (let [dlg (JDialog. frame "Open GL Information")]
    (. dlg setSize 300 300)
    
    (. dlg show)))
  
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
    (. (System/getProperties) 
       setProperty "com.apple.mrj.application.apple.menu.about.name" appName)
    (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
    (. (System/getProperties) setProperty "apple.laf.useScreenMenuBar" "true")
    (let [frame (JFrame. appName)
	  panel (GLJPanel. )
	  bar (JMenuBar.)
	  menu (JMenu. "About")]
      (.. frame getContentPane (setLayout (BorderLayout.)))
      (.. frame getContentPane (add panel))
      (. panel addGLEventListener (create_gl_event_listener))
      (. bar add menu)
      (create_menu_item "OpenGL" #(display_opengl_properties frame %) menu)
      (. frame setJMenuBar bar)
      (. frame show)
      (def main_frame frame)
      frame)))