(ns lambinator.rcgl.texture
  (:use lambinator.rcgl.util
	lambinator.util)
  (:import (javax.media.opengl GL)))

(defstruct context-texture
  :texture-spec
  :gl-handle
  :name)


(defn rcglt-allocate-opengl-texture-handle[gl]
  (rcglu-allocate-gl-item gl glGenTextures))

(defn rcglt-release-opengl-texture-handle [gl hdl]
  (rcglu-release-gl-item gl glDeleteTextures hdl))

(defn rcglt-tex2d-param
  "Set a texture 2D param.  This parameter will apply to
whichever the bound texture happens to be at this time"
  [gl param-name param-value]
  (. gl glTexParameteri GL/GL_TEXTURE_2D param-name param-value))

(defn rcglt-context-texture-valid [context-texture]
  (when (and context-texture
	     (> (context-texture :gl-handle) 0))))

(defn rcglt-create-context-texture
  "Create a named texture object with storage but do not
upload any information"
  [gl texture-spec name]
  (let [thandle (rcglt-allocate-opengl-texture-handle gl)
	rc-format (texture-spec :format)
	rc-dtype (texture-spec :datatype)
	[width height] (texture-spec :size)
	internal-format (rcglu-gl-internal-format-from-rc-format-and-type rc-format rc-dtype)
	external-format (rcglu-gl-format-from-rc-format rc-format)
	external-datatype (rcglu-gl-datatype-from-rc-datatype rc-dtype)]
    (. gl glTexImage2D GL/GL_TEXTURE_2D 0 internal-format width height 0 
       external-format external-datatype nil)
    (struct-map context-texture
      :texture-spec texture-spec
      :gl-handle thandle
      :name name)))

(defn rcglt-destroy-context-texture
  "Destroy a texture object if it is valid"
  [gl context-texture]
  (when (rcglt-context-texture-valid context-texture)
    (rcglt-release-opengl-texture-handle gl (context-texture :gl-handle)))
  (assoc context-texture :gl-handle 0))

(defn- get-and-remove[texture-map-ref name]
  (dosync
   (let [existing (@texture-map-ref name)]
     (when existing
       (alter texture-map-ref dissoc name))
     existing)))

(defn rcglt-destroy-named-texture
  "Destroy a named texture object"
  [gl texture-map-ref name]
  (let [existing (get-and-remove texture-map-ref name)]
    (when existing 
      (rcglt-destroy-context-texture gl existing))))

(defn rcglt-create-named-texture
  "Create a named texture, destroying any existing"
  [gl texture-map-ref texture-spec name]
  (rcglt-destroy-named-texture gl texture-map-ref name)
  (let [new-texture (rcglt-create-context-texture gl texture-spec name)]
    (dosync (alter texture-map-ref assoc name new-texture))))

(defn rcglt-reload-resources[gl texture-map-ref]
  (let [new-values (mapcat (fn [[name tex]]
			     [name (rcglt-create-context-texture 
				    gl
				    (tex :texture-spec)
				    name)])
			   @texture-map-ref)]
    (util-update-map-ref texture-map-ref new-values)))
