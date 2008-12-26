(in-ns 'lambinator.rcgl)

(defstruct context_texture :texture_spec :gl_handle)

(defn create_context_texture [texture_spec gl_handle]
  (struct context_texture texture_spec gl_handle))

(defstruct texture_manager :textures )

(defn create_texture_manager [textures]
  (struct texture_manager textures))
	
;takes a gl, a texture spec,
;and returns a context_texture
;This simply allocates a texture handle
(defn allocate_opengl_texture [gl texture_spec]
  (create_context_texture 
   texture_spec 
   (allocate_gl_item (fn [count args offset] (. gl glGenTextures count args offset)))))

;releases the gl handle and returns a new context texture
;with the handle value set to -1
(defn release_opengl_texture [gl context_texture]
  (let [tex_handle (context_texture :gl_handle)]
    (release_gl_item (fn [count args offset] (. gl glDeleteTextures count args offset)) tex_handle)
    (create_context_texture (context_texture :texture_spec) -1)))

;A context texture is empty if its gl handle <= 0
(defn find_empty_context_texture[textures]
  (util_find_next_matching_index textures
				 #(<= 0 (% :gl_handle)) (fn [] nil)))

(defn allocate_context_texture_item[gl textures texture_spec]
  (let [[index textures] (find_empty_context_texture textures)
	context_texture (allocate_opengl_texture gl texture_spec)]
    [(assoc textures index context_texture) index]))

(defn release_context_texture_item[gl textures index]
  (let [context_texture (textures index)]
    (assoc textures index (release_opengl_texture gl context_texture))))
    
  