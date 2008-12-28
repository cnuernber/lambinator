(ns lambinator.util
  (:import (java.lang.reflect Modifier)
	   (java.nio ByteBuffer IntBuffer FloatBuffer ShortBuffer)))

(defn find_static_fields_by_value [cls_name val]
  (let [cls (Class/forName cls_name)
	flds (. cls getFields)
	filtered (filter #(and (Modifier/isPublic (. % getModifiers))
			       (Modifier/isStatic (. % getModifiers))
			       (== (. % get nil) val))
			 flds)]
    filtered))

(defn get_static_field_value [cls_name fld_name]
  (let [cls (Class/forName cls_name)
	fld (. cls getField fld_name)]
    (when fld (. fld get nil))))

(defmacro sets! [vars & rest]  
  `(do ~@(map (fn [flds] `(set! (. ~vars ~(first flds)) ~(second flds))) (apply array-map rest))))

;find next unused item index from a vector of items.
;filter takes an item, and returns true if it is unused.
;if non are found, then the length of the items is returned.
;returns a pair of index, and items vector.
(defn util_find_next_matching_index [items filter_fn default_fn]
  (let [indexed_items (map vector items (iterate inc 0))
	matches (filter #(filter_fn (first %)) indexed_items)
	first_match (first matches)]
    (if first_match
      [(second first_match) items]
      (let [idx (count items)
	   new_items (conj items (default_fn))]
	[idx new_items]))))

(defn current_thread_id[]
  (let [thread (Thread/currentThread)]
    (. thread getId)))

(defn create_buffer_of_type[data_seq datatype buffer_object_fn]
  (let [data_array (into-array datatype data_seq)
	buffer_object (buffer_object_fn (count data_array))]
    (. buffer_object put data_array)
    buffer_object))

(defmulti make_nio_buffer_data (fn [data_seq first_data_item] (class first_data_item)))
(defmethod make_nio_buffer_data Byte [data_seq first_data_item]
  (create_buffer_of_type data_seq Byte/TYPE #(ByteBuffer/allocate %)))
(defmethod make_nio_buffer_data Short [data_seq first_data_item]
  (create_buffer_of_type data_seq Short/TYPE #(ShortBuffer/allocate %)))
(defmethod make_nio_buffer_data Integer [data_seq first_data_item]
  (create_buffer_of_type data_seq Integer/TYPE #(IntBuffer/allocate %)))
(defmethod make_nio_buffer_data Float [data_seq first_data_item]
  (create_buffer_of_type data_seq Float/TYPE #(FloatBuffer/allocate %)))
(defmethod make_nio_buffer_data :default [&args]
  nil)


;returns null if seq doesn't contain any data
(defn make_nio_buffer[data_seq]
  (let [first_item (first data_seq)]
    (if first_item
      (make_nio_buffer_data data_seq first_item)
      nil)))
  
