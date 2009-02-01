(ns lambinator.app-creator.view
  (:use (lambinator ui)
	(lambinator.app-creator.view resources))
  (:import (javax.swing JTree)))

(defstruct acv-view
  :app-frame ;ui-app-frame
  :resources ;resources palette
  )  

(defn acv-create-view
  "Should be called from the swing thread"
  [view-name]
  (let [app-frame (ui-create-app-frame view-name)
	resources (acvr-create)
	res-component (acvr-get-component resources)
	retval (struct-map acv-view
		 :app-frame app-frame
		 :resources resources)]
    (ui-add-palette app-frame res-component "Resources" "Resources" :left)
    (ui-set-frame-visible app-frame)
    retval))

(defn acv-get-app-frame[acv-view]
  (acv-view :app-frame))