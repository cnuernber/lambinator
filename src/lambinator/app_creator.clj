(. (System/getProperties) setProperty 
   "com.apple.mrj.application.apple.menu.about.name" "App Creator")
(ns lambinator.app-creator
  (:use lambinator.ui)
  (:import (javax.swing SwingUtilities))
  (:gen-class))

(defstruct ac-app
  :app-frame)

;global public variable used to find the app from
(defonce ac-active-apps (ref nil))

(defn ac-get-active-app []
  "Get a list of the active app creator applications"
  (first @ac-active-apps))

(defn ac-remove-active-app
  "Remove the given application from the list of active apps.
You probably don't need to call this, a close hook is automatically
added"
  [app-ref]
  (dosync (alter 
	   ac-active-apps 
	   #(filter 
	     (fn [cur-app-ref]
	       (not (= cur-app-ref app-ref)))
	     %))))

(defn ac-create-application
  "Create a new app creator application.  Safe to call from
any thread.  Returns a reference to the new app.  The actual
creation process happens on the swing thread."
  ([extra-hooks]
     (let [retval (ref (struct-map ac-app))]
       (dosync (alter ac-active-apps conj retval))
       (SwingUtilities/invokeLater 
	(fn []
	  (let [app-frame (ui-create-app-frame "App Creator")]
	    (ui-set-frame-visible app-frame)
	    (dosync (alter retval assoc :app-frame app-frame))
	    (ui-add-hook 
	     app-frame 
	     :close-hooks-ref
	     #(ac-remove-active-app retval)
	     )
	    (doseq [[hook-key cls-hook] extra-hooks]
	      (ui-add-hook app-frame hook-key cls-hook)))))))
  ([]
     (ac-create-application nil)))


(defn- -main[& args]
  (ac-create-application
   [[:close-hooks-ref #(System/exit 0)]]))
