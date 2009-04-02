(. (System/getProperties) setProperty 
   "com.apple.mrj.application.apple.menu.about.name" "Cool Demos")
(ns lambinator.demo
  (:use lambinator.ui
     lambinator.ui.util
     lambinator.ui.inspector
     (lambinator.demo wave basic particle functional image))
  (:import (javax.swing SwingUtilities UIManager))
  (:gen-class))

(defstruct demo-data :frame :cleanup-cur-demo-ref :cur-demo-data-ref)

(def #^{:doc "Currently all demo start functions take a frame and a 
             reference to a nil object that they fill out internally"}
  all-demos-name-fns
  [["Wave Demo" do-create-wave-demo disable-wave-demo]
   ["Basic Demo" create-basic-demo destroy-basic-demo]
   ["Particle Demo" create-particle-demo destroy-particle-demo]
   ["Functional Graphics" dmfn-create-demo-data dmfn-destroy-demo-data]
   ["Image" #(dmim-create-demo-data %1 %2) #(dmim-destroy-demo-data %1)]])

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
    (.revalidate (ui-get-inspector-panel (demo-data :frame)))
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

(defn dm-setup-all-demos
  "This needs to be run from the swing thread"
  []
  (SwingUtilities/invokeLater 
    (fn []
      (UIManager/setLookAndFeel 
       (UIManager/getSystemLookAndFeelClassName))
      (let [frame (ui-create-app-frame "Cool Demos")
            demo-data (dm-create-all-demos-menu frame)]
        (. (frame :frame) setVisible true)
        (ui-add-hook 
          frame 
          :close-hooks-ref 
          (fn [] 
            (dm-cleanup-current-demo demo-data)
            ;(System/exit 0)
	    ))))))



(defn -main [& args]
  (dm-setup-all-demos))
