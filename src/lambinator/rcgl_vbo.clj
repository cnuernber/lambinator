(in-ns 'lambinator.rcgl)

(defn rcgl-vbo-log [log-data-ref type & args]
  (when log-data-ref
    (log-message @log-data-ref "rcgl.glsl:" type args)))

;gl supports two array types, ones for data and ones for index.
(def vbo-types [:data :index])
(def vbo-datatypes [:ubyte :ushort :float])
(defstruct gl-vbo :gl-handle :name :type :generator :gl-datatype :item-count :gl-error)

(defn gl-vbo-valid[vbo]
  (and vbo
   (> (vbo :gl-handle) 0)))

(defmulti vbo-gl-type-from-vbo-type identity)
(defmethod vbo-gl-type-from-vbo-type :default [-] GL/GL_ARRAY_BUFFER)
(defmethod vbo-gl-type-from-vbo-type :index [-] GL/GL_ELEMENT_ARRAY_BUFFER)

(defn create-invalid-vbo[name vbo-type]
  (struct gl-vbo 0 name vbo-type nil))

(defmulti gl-datatype-from-clojure-type #(class %))
(defmethod gl-datatype-from-clojure-type :default [-] GL/GL_FLOAT)
(defmethod gl-datatype-from-clojure-type Short/TYPE [-] GL/GL_SHORT)
(defmethod gl-datatype-from-clojure-type Byte/TYPE [-] GL/GL_BYTE)
(defmethod gl-datatype-from-clojure-type Integer/TYPE [-] GL/GL_INT)

(defmulti item-size-from-clojure-type #(class %))
(defmethod item-size-from-clojure-type :default [-] 4)
(defmethod item-size-from-clojure-type Short/TYPE [-] 2)
(defmethod item-size-from-clojure-type Byte/TYPE [-] 1)

;datatype is one of the rc datatypes, ubyte ushort or float 
(defn create-gl-vbo [log-data-ref gl name vbo-type data-seq generator]
  (let [data-buffer (make-nio-buffer data-seq)
	new-vbo (struct gl-vbo 0 name vbo-type generator)]
    (if data-buffer
      (do
	(rcgl-fbo-log log-data-ref :info "Creating vbo: " name)
	
	(let [vbo-handle (allocate-gl-item (fn [count args offset] (. gl glGenBuffers count args offset)))
	      vbo-gl-type (vbo-gl-type-from-vbo-type vbo-type)
	      gl-datatype (gl-datatype-from-clojure-type (first data-seq))
	      item-count (. data-buffer limit)
	      data-size (* item-count (item-size-from-clojure-type (first data-seq)))]
	  (. gl glBindBuffer vbo-gl-type vbo-handle)
	  (. gl glBufferData vbo-gl-type data-size data-buffer GL/GL_STATIC_DRAW )
	  (assoc new-vbo 
	    :gl-handle vbo-handle 
	    :gl-datatype gl-datatype 
	    :item-count item-count 
	    :gl-error (get-gl-error gl))))
      new-vbo)))

(defn delete-gl-vbo[log-data-ref gl vbo]
  (when (gl-vbo-valid vbo)
    (rcgl-fbo-log log-data-ref :info "deleting vbo: " (vbo :name))
    (release-gl-item (fn [count args offset] (. gl glDeleteBuffers count args offset)) (vbo :gl-handle)))
  (assoc vbo :gl-handle 0))

(defn add-new-vbo [gl vbos-ref name vbo-type ]
  (dosync
   (let [existing (@vbos-ref name)]
     (ref-set vbos-ref (assoc @vbos-ref name (create-invalid-vbo name vbo-type)))
     existing)))	 

(defn get-and-remove-vbo [vbos-ref name]
  (dosync
   (let [existing (@vbos-ref name)]
     (ref-set vbos-ref (dissoc @vbos-ref name))
     existing)))

(defn update-vbo[log-data-ref gl vbos-ref name generator]
  (let [existing (@vbos-ref name)]
    (when existing
      (let [vbo-type (existing :type)
	    generate-seq (generator)
	    new-vbo (create-gl-vbo log-data-ref gl name vbo-type generate-seq generator)]
	(dosync (ref-set vbos-ref (assoc @vbos-ref name new-vbo)))
	(delete-gl-vbo log-data-ref gl existing)))))

(defn create-vbo [log-data-ref gl vbos-ref name vbo-type generator]
  (let [existing (add-new-vbo gl vbos-ref name vbo-type)
	matches-exactly (and existing
			     (= (existing :vbo-type) vbo-type))]
    (when (not matches-exactly)
      (when existing
	(delete-gl-vbo log-data-ref gl existing))
      (update-vbo log-data-ref gl vbos-ref name generator))))

(defn delete-vbo [log-data-ref gl vbos-ref name]
  (let [existing (get-and-remove-vbo vbos-ref name)]
    (when existing
      (delete-gl-vbo log-data-ref gl existing))))

;called when all of the system resources need to be rebooted.
(defn vbo-resources-destroyed[log-data-ref gl vbos-ref]
  (let [new-vbos (mapcat (fn [[name vbo]]
			   (let [generator (vbo :generator)
				 data-seq (generator)
				 vbo-type (vbo :vbo-type)
				 new-vbo (create-gl-vbo log-data-ref gl (vbo :name) vbo-type data-seq generator)]
			     [name new-vbo]))
			 @vbos-ref)]
    (when new-vbos
      (dosync (ref-set vbos-ref (apply assoc @vbos-ref new-vbos))))))


(defstruct vbo-manager :vbos-ref) ;vbos mapped to names
(defn create-vbo-manager [] (struct vbo-manager (ref {})))
				 