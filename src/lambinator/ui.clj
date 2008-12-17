(ns lambinator.ui
  (:import (com.trolltech.qt.gui QApplication QMainWindow)))

(defn ensure_app_init []
  (try(QApplication/initialize (make-array String 0))(catch RuntimeException e# (println e#))))

(def #^{:private true} exec_var 1)

(defn exec []
  (def #^{:private true} exec_var 1)
  (while 
      (== exec_var 1)
    (QApplication/processEvents)
    (Thread/sleep 10)))
  
(defn exit_exec []
  (def #^{:private true} exec_var 0))

(defn create_app_frame []
  (ensure_app_init)
  (let [mainWin (new QMainWindow)
	mbar (. mainWin menuBar)
	menu (. mbar addMenu "Repl")
	action (. menu addAction "Back to Repl")]
    (.. action triggered (connect #(exit_exec) "call()"))
    (. mainWin show)
    mainWin))		      ;return the button for further reference