(ns lambinator.mockgl
  (:import (javax.media.opengl GL))
  (:use (lambinator.rcgl)))


(defstruct gl_mock_context
  :textures
  :fbos)

;create an empty gl context with nothing in it.
(defn create_gl_mock_context []
  (struct gl_mock_context [] []))

;produces a new vector of textures
(defn mock_glGenTextures [context_textures number textures_result offset]
  (reduce (fn [context_textures index] 
	    (aset textures_result offset+index (count textures))
	    (conj context_textures nil))
	  textures 
	  (take number (iterate #(+ % 1)))))
	      


