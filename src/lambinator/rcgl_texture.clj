(in-ns 'lambinator.rcgl)

(defn allocate-opengl-texture-handle[gl]
  (rcglu-allocate-gl-item gl glGenTextures))

(defn release-opengl-texture-handle [gl hdl]
  (rcglu-release-gl-item gl glDeleteTextures hdl))