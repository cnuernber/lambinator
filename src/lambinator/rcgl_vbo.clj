(in-ns 'lambinator.rcgl)

(defn rcgl_vbo_log [log_data_ref type & args]
  (when log_data_ref
    (log_message @log_data_ref "rcgl.glsl:" type args)))

;gl supports two array types, ones for data and ones for index.
(def vbo_types [:data :index])
(def vbo_datatypes [:ubyte :ushort :float])
(defstruct gl_vbo :gl_handle :name :type :generator :gl_datatype :item_count)

(defn gl_vbo_valid[vbo]
  (and vbo
   (> (vbo :gl_handle) 0)))

(defmulti vbo_gl_type_from_vbo_type identity)
(defmethod vbo_gl_type_from_vbo_type :default [_] GL/GL_ARRAY_BUFFER)
(defmethod vbo_gl_type_from_vbo_type :index [_] GL/GL_ELEMENT_ARRAY_BUFFER)

(defn create_invalid_vbo[name vbo_type]
  (struct gl_vbo 0 name vbo_type nil))

(defmulti gl_datatype_from_clojure_type #(class %))
(defmethod gl_datatype_from_clojure_type :default [_] GL/GL_FLOAT)
(defmethod gl_datatype_from_clojure_type Short/TYPE [_] GL/GL_SHORT)
(defmethod gl_datatype_from_clojure_type Byte/TYPE [_] GL/GL_BYTE)
(defmethod gl_datatype_from_clojure_type Integer/TYPE [_] GL/GL_INT)

(defmulti item_size_from_clojure_type #(class %))
(defmethod item_size_from_clojure_type :default [_] 4)
(defmethod item_size_from_clojure_type Short/TYPE [_] 2)
(defmethod item_size_from_clojure_type Byte/TYPE [_] 1)

;datatype is one of the rc datatypes, ubyte ushort or float 
(defn create_gl_vbo [log_data_ref gl name vbo_type data_seq generator]
  (let [data_buffer (make_nio_buffer data_seq)
	new_vbo (struct gl_vbo 0 name vbo_type generator)]
    (if data_buffer
      (do
	(rcgl_fbo_log log_data_ref :info "Creating vbo: " name)
	
	(let [vbo_handle (allocate_gl_item (fn [count args offset] (. gl glGenBuffers count args offset)))
	      vbo_gl_type (vbo_gl_type_from_vbo_type vbo_type)
	      gl_datatype (gl_datatype_from_clojure_type (first data_seq))
	      item_count (. data_buffer limit)
	      data_size (* item_count (item_size_from_clojure_type (first data_seq)))]
	  (. gl glBindBuffer vbo_gl_type vbo_handle)
	  (. gl glBufferData vbo_gl_type data_size data_buffer GL/GL_STATIC_DRAW )
	  (assoc new_vbo :gl_handle vbo_handle :gl_datatype gl_datatype :item_count item_count)))
      new_vbo)))

(defn delete_gl_vbo[log_data_ref gl vbo]
  (when (gl_vbo_valid vbo)
    (rcgl_fbo_log log_data_ref :info "deleting vbo: " (vbo :name))
    (release_gl_item (fn [count args offset] (. gl glDeleteBuffers count args offset)) (vbo :gl_handle)))
  (assoc vbo :gl_handle 0))

(defn add_new_vbo [gl vbos_ref name vbo_type ]
  (dosync
   (let [existing (@vbos_ref name)]
     (ref-set vbos_ref (assoc @vbos_ref name (create_invalid_vbo name vbo_type)))
     existing)))	 

(defn get_and_remove_vbo [vbos_ref name]
  (dosync
   (let [existing (@vbos_ref name)]
     (ref-set vbos_ref (dissoc @vbos_ref name))
     existing)))

(defn update_vbo[log_data_ref gl vbos_ref name generator]
  (let [existing (@vbos_ref name)]
    (when existing
      (let [vbo_type (existing :type)
	    generate_seq (generator)
	    new_vbo (create_gl_vbo log_data_ref gl name vbo_type generate_seq generator)]
	(dosync (ref-set vbos_ref (assoc @vbos_ref name new_vbo)))
	(delete_gl_vbo log_data_ref gl existing)))))

(defn create_vbo [log_data_ref gl vbos_ref name vbo_type generator]
  (let [existing (add_new_vbo gl vbos_ref name vbo_type)]
    (when existing
      (delete_gl_vbo log_data_ref gl existing))
    (update_vbo log_data_ref gl vbos_ref name generator)))

(defn delete_vbo [log_data_ref gl vbos_ref name]
  (let [existing (get_and_remove_vbo vbos_ref name)]
    (when existing
      (delete_gl_vbo log_data_ref gl existing))))

;called when all of the system resources need to be rebooted.
(defn vbo_resources_destroyed[log_data_ref gl vbos_ref]
  (let [new_vbos (mapcat (fn [[name vbo]]
			   (let [generator (vbo :generator)
				 data_seq (generator)
				 vbo_type (vbo :vbo_type)
				 new_vbo (create_gl_vbo log_data_ref gl (vbo :name) vbo_type data_seq generator)]
			     [name new_vbo]))
			 @vbos_ref)]
    (when new_vbos
      (dosync (ref-set vbos_ref (apply assoc @vbos_ref new_vbos))))))


(defstruct vbo_manager :vbos_ref) ;vbos mapped to names
(defn create_vbo_manager [] (struct vbo_manager (ref {})))
				 