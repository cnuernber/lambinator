(ns lambinator.ui.util
  (:import (javax.swing JTextPane JScrollPane JMenuItem)
	   (javax.swing.text.html HTMLEditorKit)
	   (java.awt.event ActionListener)))

(defn uiut-create-html-label
  "Create a label that you can cut or copy from and
that is capable of displaying html data"
  ([label-text]
  (let [field (JTextPane.)]
    (try 
     (. field setEditorKit (HTMLEditorKit. ))
     (catch NullPointerException e)) ;don't care
    (doto field
      (.setText label-text)
      (.setBorder nil)
      (.setOpaque false)
      (.setEditable false))
    field))
  ([]
     (uiut-create-html-label nil)))


(defn uiut-create-action-listener [lmbda]
  (proxy [Object ActionListener]
      []
    (actionPerformed 
     [evt]
     (apply lmbda [evt]))))

(defn uiut-create-menu-item [title lmbda parent]
  (let [item (JMenuItem. title)]
    (. item addActionListener (uiut-create-action-listener lmbda))
    (. parent add item)
    item))
