(. (System/getProperties) setProperty 
   "com.apple.mrj.application.apple.menu.about.name" "Cool Demos")
(ns lambinator.demo
  (:use lambinator.ui
	lambinator.ui.util
	lambinator.ui.inspector
	lambinator.demo.wave )
  (:import (javax.swing SwingUtilities))
  (:gen-class))

(defstruct demo-data :frame :cleanup-cur-demo-ref :cur-demo-data-ref)

(def #^{:doc "Currently all demo start functions take a frame and a 
reference to a nil object that they fill out internally"}
     all-demos-name-fns
     [["Wave Demo" do-create-wave-demo disable-wave-demo]
      ["Functional Graphics" nil nil]])

(defn dm-cleanup-current-demo [demo-data]
  (let [cleanup-fn @(demo-data :cleanup-cur-demo-ref)
	sub-demo-data @(demo-data :cur-demo-data-ref)]
    (when cleanup-fn
      (cleanup-fn sub-demo-data))
    (uii-setup-inspector-panel 
     (ui-get-inspector-panel (demo-data :frame)) nil)
    (dosync
     (ref-set (demo-data :cleanup-cur-demo-ref) nil)
     (ref-set (demo-data :cur-demo-data-ref) nil))))

(defn- switch-to-demo[demo-data create destroy]
  (let [sub-data (ref nil)
	frame (demo-data :frame)]
    (when create
      (create frame sub-data))
    ;ensure an update of the inspector panel
    (.repaint (ui-get-inspector-panel (demo-data :frame)))
    (dosync
     (ref-set (demo-data :cleanup-cur-demo-ref) destroy)
     (ref-set (demo-data :cur-demo-data-ref) sub-data))))

(defn dm-create-all-demos-menu
  "Create the demo menu item and add demos under the menu item.
Assumes you are running in the swing thread"
  [frame]
  (let [demo-data (struct-map demo-data
		    :cleanup-cur-demo-ref (ref nil)
		    :cur-demo-data-ref (ref nil)
		    :frame frame)
	menu-item (ui-create-top-level-menu-item frame "Demos")]
    (doseq [[name create destroy] all-demos-name-fns]
      (let [menu-fn (fn [_]
			(dm-cleanup-current-demo demo-data)
			(switch-to-demo demo-data create destroy))]
	
	(uiut-create-menu-item name menu-fn menu-item)))
    demo-data))


(defn- -main [& args]
  (SwingUtilities/invokeLater (fn []
				(let [frame (ui-create-app-frame "Cool Demos")
				      demo-data (dm-create-all-demos-menu frame)]
				  (. (frame :frame) setVisible true)
				  (ui-add-hook 
				   frame 
				   :close-hooks-ref 
				   (fn [] 
				     (dm-cleanup-current-demo demo-data)
				     (System/exit 0)))))))