(ns lambinator.ui
  (:import (com.trolltech.qt.gui QApplication QMainWindow)
	   (com.trolltech.qt.opengl QGLWidget)
	   (com.trolltech.demos LambdaGLWidget)
	   ))

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

(defn main_win_close [closeEvt]
  (exit_exec)
  (println "Lambinator Main Closed")
  (. closeEvt ignore))
  

(defn create_lambinator_main []
  (proxy [QMainWindow] 
      []
    (closeEvent 
     ([evt] 
	(main_win_close evt)
	))))
(def glContext nil)
(def glObj nil)
(def glFactory nil)

(defn widget_init_gl []
  ;(GL11/glClear 0)
  (println "gl initialized"))

(defn widget_resize_gl [x y]
  (println "gl resized: " x "," y ))

(defn widget_paint_gl []
  (println "gl painted"))

(defn create_gl_widget []
  (proxy [QGLWidget]
      []
    (initializeGL 
     []
     (widget_init_gl))
    (resizeGL 
     [x y]
     (widget_resize_gl x y))
    (paintGL
     []
     (widget_paint_gl))))

(defn create_app_frame []
  (ensure_app_init)
  (let [mainWin (create_lambinator_main)
	mbar (. mainWin menuBar)
	menu (. mbar addMenu "Repl")
	glitem (LambdaGLWidget. nil)
	action (. menu addAction "Back to Repl")]
    (.. action triggered (connect #(exit_exec) "call()"))
    (doto mainWin
      (.setWindowTitle "Lambdafication")
      (.setCentralWidget glitem)
      (.show))
    mainWin))		      ;return the button for further reference
