(ns clojure-qt4
  (:import (com.trolltech.qt.gui QApplication QPushButton QFont QFont$Weight)))

(defn ensure_app_init []
  (try(QApplication/initialize (make-array String 0))(catch RuntimeException e# (println e#))))

(def exec_var 1)

(defn exec []
  (def exec_var 1)
    (while 
	(== exec_var 1)
      (QApplication/processEvents)
      (Thread/sleep 10)))

(defn create_app_frame []
  (ensure_app_init)
   (let [app (QApplication/instance)
         button (new QPushButton "Go Clojure Go")]
     (.. button clicked (connect (fn [] (def exec_var 0)) "call()"))
     (doto button
       (.resize 250 100)
       (.setFont (new QFont "Deja Vu Sans" 18 (.. QFont$Weight Bold value)))
       (.setWindowTitle "Go Clojure Go")
       (.show))
     button)) ;return the button for further reference