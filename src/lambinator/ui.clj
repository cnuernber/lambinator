(ns lambinator.ui)

(def gl_system_lookup_strs
     '("GL_EXTENSIONS" 
       "GL_VENDOR"
       "GL_RENDERER"
       "GL_VERSION"))

(def gl_todo_list (ref nil))

(def gl_system_strs (ref {}))

(def main_frame nil)

(load "ui_defs")