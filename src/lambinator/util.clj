(ns lambinator.util
  (:import (java.lang.reflect Modifier)
	   (java.nio ByteBuffer IntBuffer FloatBuffer ShortBuffer)
	   (java.util.regex Pattern)
	   (java.io File))
  (:use clojure.contrib.except))

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

;These are used pirmarily for ui stuff
(defmacro sets! [vars & rest]  
  `(do ~@(map (fn [flds] `(set! (. ~vars ~(first flds)) ~(second flds))) (apply array-map rest))))

(defmacro add_with_constraints [item constraints parent & rest]
	`(do (sets! ~constraints ~@rest) (. ~parent add ~item ~constraints)))

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
    ;set position back to 0, ready for channel read
    ;this was a difficult one to figure out.
    (. buffer_object flip)
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

;This does not put spaces in things.  It is meant for
;literal stringification
(defn stringify [& args]
  (let [builder (StringBuilder. )]
    (doseq [arg args]
      (. builder append (str arg)))
    (. builder toString)))

(defn throw_if_item_missing [item coll & args]
  (when (not (some (partial = item) coll))
    (throwf (stringify args))))

(defn split_on_newline [str]
  (if str
    (re-seq (Pattern/compile "[^\n]+") str)
    nil))

(defn split_on_whitespace [str]
  (if str
    (re-seq (Pattern/compile "\\S+") str)
    nil))

(defn run_cmd 
  ([cmd_and_args_seq working_dir]
     (let [working_dir (if working_dir
			 working_dir
			 (System/getProperty "user.dir"))]
       (. (Runtime/getRuntime) exec (into-array String cmd_and_args_seq) nil (File. working_dir))))
  ([cmd_and_args]
     (run_cmd cmd_and_args nil)))