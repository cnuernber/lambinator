(in-ns 'lambinator.rcgl)

;gl supports two array types, ones for data and ones for index.
(def vbo_types [:data :index])
(def vbo_datatypes [:ubyte :ushort :float])
(defstruct gl_vbo :gl_handle :name :type)

(defn gl_vbo_valid[vbo]
  (> (vbo :gl_handle) 0))

(defmulti vbo_gl_type_from_vbo_type identity)
(defmethod vbo_gl_type_from_vbo_type :default [_] GL/GL_ARRAY_BUFFER)
(defmethod vbo_gl_type_from_vbo_type :index [_] GL/GL_ELEMENT_ARRAY_BUFFER)


;datatype is one of the rc datatypes, ubyte ushort or float 
(defn create_gl_vbo [gl name vbo_type data_seq]
  (println "Creating vbo: " name)
  (let [data_buffer (make_nio_buffer data_seq)]
    (if data_buffer
      (do
	(let [vbo_handle (allocate_gl_item (fn [count args offset] (. gl glGenBuffers count args offset)))
	      vbo_gl_type (vbo_gl_type_from_vbo_type vbo_type)]
	  (. gl glBindBuffer vbo_gl_type vbo_handle)
	  (. gl glBufferData vbo_gl_type (. data_buffer position) data_buffer GL/GL_STATIC_DRAW )
	  (struct gl_vbo vbo_handle name vbo_type)))
      (struct gl_vbo 0 name vbo_type))))

(defn delete_gl_fbo[gl vbo]
  (when (gl_vbo_valid vbo)
    (release_gl_item (fn [count args offset] (. gl glDeleteBuffers count args offset)) (vbo :gl_handle)))
  (assoc vbo :gl_handle 0))
    
	
  