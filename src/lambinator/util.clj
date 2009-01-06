(ns lambinator.util
  (:import (java.lang.reflect Modifier)
	   (java.nio ByteBuffer IntBuffer FloatBuffer ShortBuffer)
	   (java.util.regex Pattern)
	   (java.io File))
  (:use clojure.contrib.except))

(defn find-static-fields-by-value [cls-name val]
  (let [cls (Class/forName cls-name)
	flds (. cls getFields)
	filtered (filter #(and (Modifier/isPublic (. % getModifiers))
			       (Modifier/isStatic (. % getModifiers))
			       (== (. % get nil) val))
			 flds)]
    filtered))

(defn get-static-field-value [cls-name fld-name]
  (let [cls (Class/forName cls-name)
	fld (. cls getField fld-name)]
    (when fld (. fld get nil))))

;These are used pirmarily for ui stuff
(defmacro sets! [vars & rest]  
  `(do ~@(map (fn [flds] `(set! (. ~vars ~(first flds)) ~(second flds))) (apply array-map rest))))

(defmacro add-with-constraints [item constraints parent & rest]
	`(do (sets! ~constraints ~@rest) (. ~parent add ~item ~constraints)))

;find next unused item index from a vector of items.
;filter takes an item, and returns true if it is unused.
;if non are found, then the length of the items is returned.
;returns a pair of index, and items vector.
(defn util-find-next-matching-index [items filter-fn default-fn]
  (let [indexed-items (map vector items (iterate inc 0))
	matches (filter #(filter-fn (first %)) indexed-items)
	first-match (first matches)]
    (if first-match
      [(second first-match) items]
      (let [idx (count items)
	   new-items (conj items (default-fn))]
	[idx new-items]))))

(defn current-thread-id[]
  (let [thread (Thread/currentThread)]
    (. thread getId)))

(defn create-buffer-of-type[data-seq datatype buffer-object-fn]
  (let [data-array (into-array datatype data-seq)
	buffer-object (buffer-object-fn (count data-array))]
    (. buffer-object put data-array)
    ;set position back to 0, ready for channel read
    ;this was a difficult one to figure out.
    (. buffer-object flip)
    buffer-object))

(defmulti make-nio-buffer-data (fn [data-seq first-data-item] (class first-data-item)))
(defmethod make-nio-buffer-data Byte [data-seq first-data-item]
  (create-buffer-of-type data-seq Byte/TYPE #(ByteBuffer/allocate %)))
(defmethod make-nio-buffer-data Short [data-seq first-data-item]
  (create-buffer-of-type data-seq Short/TYPE #(ShortBuffer/allocate %)))
(defmethod make-nio-buffer-data Integer [data-seq first-data-item]
  (create-buffer-of-type data-seq Integer/TYPE #(IntBuffer/allocate %)))
(defmethod make-nio-buffer-data Float [data-seq first-data-item]
  (create-buffer-of-type data-seq Float/TYPE #(FloatBuffer/allocate %)))
(defmethod make-nio-buffer-data :default [&args]
  nil)


;returns null if seq doesn't contain any data
(defn make-nio-buffer[data-seq]
  (let [first-item (first data-seq)]
    (if first-item
      (make-nio-buffer-data data-seq first-item)
      nil)))

;This does not put spaces in things.  It is meant for
;literal stringification
(defn stringify [& args]
  (let [builder (StringBuilder. )]
    (doseq [arg args]
      (. builder append (str arg)))
    (. builder toString)))

(defn throw-if-item-missing [item coll & args]
  (when (not (some (partial = item) coll))
    (throwf (stringify args))))

(defn split-on-newline [str]
  (if str
    (re-seq (Pattern/compile "[^\n]+") str)
    nil))

(defn split-on-whitespace [str]
  (if str
    (re-seq (Pattern/compile "\\S+") str)
    nil))

(defn run-cmd 
  ([cmd-and-args-seq working-dir]
     (let [working-dir (if working-dir
			 working-dir
			 (System/getProperty "user.dir"))]
       (. (Runtime/getRuntime) exec (into-array String cmd-and-args-seq) nil (File. working-dir))))
  ([cmd-and-args]
     (run-cmd cmd-and-args nil)))