(ns lambinator.rcgl.vbo
  (:use lambinator.rcgl.util lambinator.log
	lambinator.util)
  (:import (javax.media.opengl GL)))

(defn- rcgl-vbo-log [log-data-ref type & args]
  (when log-data-ref
    (log-message @log-data-ref "rcgl.glsl:" type args)))

;gl supports two array types, ones for data and ones for index.
(def vbo-types [:data :index])
(def vbo-datatypes [:ubyte :ushort :float])
(defstruct gl-vbo :gl-handle :name :type :generator :gl-datatype :item-count :gl-error)

(defn rcglv-vbo-valid
  "Return true if this is a valid vbo"
  [vbo]
  (and vbo
   (> (vbo :gl-handle) 0)))

(defmulti rcglv-gl-type-from-vbo-type identity)
(defmethod rcglv-gl-type-from-vbo-type :default [_] GL/GL_ARRAY_BUFFER)
(defmethod rcglv-gl-type-from-vbo-type :index [_] GL/GL_ELEMENT_ARRAY_BUFFER)

(defn- create-invalid-vbo[name vbo-type]
  (struct gl-vbo 0 name vbo-type nil))

;datatype is one of the rc datatypes, ubyte ushort or float 
(defn rcglv-create-vbo 
  "Create a rcglv vbo from a data sequence and a generator function"
  [log-data-ref gl name vbo-type data-seq generator]
  (let [data-buffer (util-make-nio-buffer data-seq)
	new-vbo (struct gl-vbo 0 name vbo-type generator)]
    (if data-buffer
      (do
	(rcgl-vbo-log log-data-ref :info "Creating vbo: " name)	
	(let [vbo-handle (rcglu-allocate-gl-item gl glGenBuffers)
	      vbo-gl-type (rcglv-gl-type-from-vbo-type vbo-type)
	      gl-datatype (rcglu-gl-datatype-from-clojure-type (first data-seq))
	      item-count (. data-buffer limit)
	      data-size (* item-count (rcglu-gl-item-byte-size gl-datatype))]
	  (. gl glBindBuffer vbo-gl-type vbo-handle)
	  (. gl glBufferData vbo-gl-type data-size data-buffer GL/GL_STATIC_DRAW )
	  (assoc new-vbo 
	    :gl-handle vbo-handle 
	    :gl-datatype gl-datatype 
	    :item-count item-count 
	    :gl-error (rcglu-get-gl-error gl))))
      new-vbo)))

(defn rcglv-delete-vbo
  "Delete a given vbo, returning an invalid vbo"
  [log-data-ref gl vbo]
  (when (rcglv-vbo-valid vbo)
    (rcgl-vbo-log log-data-ref :info "deleting vbo: " (vbo :name))
    (rcglu-release-gl-item  gl glDeleteBuffers (vbo :gl-handle)))
  (assoc vbo :gl-handle 0))

(defn- add-new-vbo [gl vbos-ref name vbo-type ]
  (dosync
   (let [existing (@vbos-ref name)]
     (ref-set vbos-ref (assoc @vbos-ref name (create-invalid-vbo name vbo-type)))
     existing)))	 

(defn- get-and-remove-vbo [vbos-ref name]
  (dosync
   (let [existing (@vbos-ref name)]
     (ref-set vbos-ref (dissoc @vbos-ref name))
     existing)))

(defn- update-vbo[log-data-ref gl vbos-ref name generator]
  (let [existing (@vbos-ref name)]
    (when existing
      (let [vbo-type (existing :type)
	    generate-seq (generator)
	    new-vbo (rcglv-create-vbo log-data-ref gl name vbo-type generate-seq generator)]
	(dosync (ref-set vbos-ref (assoc @vbos-ref name new-vbo)))
	(rcglv-delete-vbo log-data-ref gl existing)))))

(defn rcglv-create-named-vbo [log-data-ref gl vbos-ref name vbo-type generator]
  (let [existing (add-new-vbo gl vbos-ref name vbo-type)
	matches-exactly (and existing
			     (= (existing :vbo-type) vbo-type))]
    (when (not matches-exactly)
      (when existing
	(rcglv-delete-vbo log-data-ref gl existing))
      (update-vbo log-data-ref gl vbos-ref name generator))))

(defn rcglv-delete-named-vbo [log-data-ref gl vbos-ref name]
  (let [existing (get-and-remove-vbo vbos-ref name)]
    (when existing
      (rcglv-delete-vbo log-data-ref gl existing))))

;called when all of the system resources need to be rebooted.
(defn rcglv-vbo-resources-destroyed[log-data-ref gl vbos-ref]
  (let [new-vbos (mapcat (fn [[name vbo]]
			   (let [generator (vbo :generator)
				 data-seq (generator)
				 vbo-type (vbo :vbo-type)
				 new-vbo (rcglv-create-vbo log-data-ref gl (vbo :name) vbo-type data-seq generator)]
			     [name new-vbo]))
			 @vbos-ref)]
    (when new-vbos
      (dosync (ref-set vbos-ref (apply assoc @vbos-ref new-vbos))))))
				 