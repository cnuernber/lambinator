(ns lambinator.ui.util
  (:import (javax.swing JTextPane JScrollPane JMenuItem)
	   (javax.swing.text.html HTMLEditorKit)
	   (javax.swing.event MouseInputListener)
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

(defn- call-mouse-callback
  [mouseCallbacks keyword event]
  (let [callback (mouseCallbacks keyword)]
    (when callback
      (callback event))))

(defn uiut-create-mouse-input-listener
  "Create a mouse motion listener
mouseCallbacks is a map of keyword to callback, all callbacks
take 1 argument which is the java.awt.event.MouseEvent.
Keywords are:
:click
:enter
:leave
:press
:release

:drag
:move"
  [mouseCallbacks]
  (let [do-cback (fn [keyword evt] (call-mouse-callback mouseCallbacks keyword evt))]
    (proxy [MouseInputListener] 
	[]
      (mouseClicked [evt] (call-mouse-callback mouseCallbacks :click evt))
      (mouseEntered [evt] (call-mouse-callback mouseCallbacks :enter evt))
      (mouseExited [evt] (call-mouse-callback mouseCallbacks :leave evt))
      (mousePressed [evt] (call-mouse-callback mouseCallbacks :press evt))
      (mouseReleased [evt] (call-mouse-callback mouseCallbacks :release evt))
      (mouseDragged [evt] (call-mouse-callback mouseCallbacks :drag evt))
      (mouseMoved [evt] (call-mouse-callback mouseCallbacks :move evt)))))



		    
      