(in-ns 'lambinator.rcgl)

(defstruct context-texture :texture-spec :gl-handle)

(defn create-context-texture [texture-spec gl-handle]
  (struct context-texture texture-spec gl-handle))

(defstruct texture-manager :textures )

(defn create-texture-manager []
  (struct texture-manager []))

(defn allocate-opengl-texture-handle[gl]
  (allocate-gl-item (fn [count args offset] (. gl glGenTextures count args offset))))

(defn release-opengl-texture-handle [gl hdl]
  (release-gl-item (fn [count args offset] (. gl glDeleteTextures count args offset)) hdl))
	
;takes a gl, a texture spec,
;and returns a context-texture
;This simply allocates a texture handle
(defn allocate-opengl-texture [gl texture-spec]
  (create-context-texture 
   texture-spec 
   (allocate-opengl-texture-handle gl)
   ))

;releases the gl handle and returns a new context texture
;with the handle value set to -1
(defn release-opengl-texture [gl context-texture]
  (let [tex-handle (context-texture :gl-handle)]
    (release-opengl-texture-handle gl tex-handle)
    (create-context-texture (context-texture :texture-spec) -1)))

;A context texture is empty if its gl handle <= 0
(defn find-empty-context-texture[textures]
  (util-find-next-matching-index textures
				 #(<= 0 (% :gl-handle)) (fn [] nil)))

(defn allocate-context-texture-item[gl textures texture-spec]
  (let [[index textures] (find-empty-context-texture textures)
	context-texture (allocate-opengl-texture gl texture-spec)]
    [(assoc textures index context-texture) index]))

(defn release-context-texture-item[gl textures index]
  (let [context-texture (textures index)]
    (assoc textures index (release-opengl-texture gl context-texture))))
    
  