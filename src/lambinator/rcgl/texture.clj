(ns lambinator.rcgl.texture
  (:use lambinator.rcgl.util)
  (:import (javax.media.opengl GL)))


(defn rcglt-allocate-opengl-texture-handle[gl]
  (rcglu-allocate-gl-item gl glGenTextures))

(defn rcglt-release-opengl-texture-handle [gl hdl]
  (rcglu-release-gl-item gl glDeleteTextures hdl))

(defn rcglt-tex2d-param
  "Set a texture 2D param.  This parameter will apply to
whichever the bound texture happens to be at this time"
  [gl param-name param-value]
  (. gl glTexParameteri GL/GL_TEXTURE_2D param-name param-value))