(ns lambinator.util
  (:import (java.lang.reflect Modifier)
	   (java.nio ByteBuffer IntBuffer FloatBuffer ShortBuffer)
	   (java.util.regex Pattern)
	   (java.io File))
  (:use clojure.contrib.except
	clojure.contrib.seq-utils))

(defn util-find-static-fields-by-value 
  "Given a classname and a value, return a list of static fields that match
the given value.  Note that the input is the name of the class, not an
instance or the class object itself."
  [cls-name val]
  (let [cls (Class/forName cls-name)
	flds (. cls getFields)
	filtered (filter #(and (Modifier/isPublic (. % getModifiers))
			       (Modifier/isStatic (. % getModifiers))
			       (== (. % get nil) val))
			 flds)]
    filtered))

(defn util-get-static-field-value 
  "Return the value of a static field of the given class name."
  [cls-name fld-name]
  (let [cls (Class/forName cls-name)
	fld (. cls getField fld-name)]
    (when fld (. fld get nil))))

;These are used pirmarily for ui stuff
(defmacro sets! 
  "Set multiple fields to a given value.  So if your object has fields
'weightx' 'weighty' and 'anchor', then you would call this macro like such:
(sets! obj weightx 1 weighty 2 ancher NORTH)"
  [vars & rest]  
  `(do ~@(map (fn [flds] `(set! (. ~vars ~(first flds)) ~(second flds))) (apply array-map rest))))

(defmacro util-add-with-constraints 
  "Used in conjunction with the sets! macro.  This macro is meant to be
used with GridBagLayout and GridBagConstraints.
example: util-add-with-constraints item constraints parent weightx 1.0 weighty 0.0"
  [item constraints parent & rest]
  `(do (sets! ~constraints ~@rest) (. ~parent add ~item ~constraints)))

(defn util-get-current-thread-id
  "Return the id of the currently executing thread"
  []
  (let [thread (Thread/currentThread)]
    (. thread getId)))

(defn util-create-buffer-of-type
  "Given a sequence, a datatype, and a function that returns a new
buffer object, fill the buffer object with the data in the sequence"
  [data-seq datatype buffer-object-fn]
  (let [data-array (into-array datatype data-seq)
	buffer-object (buffer-object-fn (count data-array))]
    (. buffer-object put data-array)
    ;set position back to 0, ready for channel read
    ;this was a difficult one to figure out.
    (. buffer-object flip)
    buffer-object))

(defmulti util-make-nio-buffer-data (fn [data-seq first-data-item] (class first-data-item)))
(defmethod util-make-nio-buffer-data Byte [data-seq first-data-item]
  (util-create-buffer-of-type data-seq Byte/TYPE #(ByteBuffer/allocate %)))
(defmethod util-make-nio-buffer-data Short [data-seq first-data-item]
  (util-create-buffer-of-type data-seq Short/TYPE #(ShortBuffer/allocate %)))
(defmethod util-make-nio-buffer-data Integer [data-seq first-data-item]
  (util-create-buffer-of-type data-seq Integer/TYPE #(IntBuffer/allocate %)))
(defmethod util-make-nio-buffer-data Float [data-seq first-data-item]
  (util-create-buffer-of-type data-seq Float/TYPE #(FloatBuffer/allocate %)))
(defmethod util-make-nio-buffer-data :default [&args]
  nil)


;returns null if seq doesn't contain any data
(defn util-make-nio-buffer
  "Given a sequence return a buffer object using the datatype of the first
item in the sequence as the buffer object type.  Currently capable of returning
ByteBuffer,ShortBuffer,IntBuffer, and FloatBuffer"
  [data-seq]
  (let [first-item (first data-seq)]
    (if first-item
      (util-make-nio-buffer-data data-seq first-item)
      nil)))

;This does not put spaces in things.  It is meant for
;literal stringification
(defn util-stringify 
  "Create a string from the given arguments.  Unlike print, do not put any spaces
into the output for list items"
  [& args]
  (let [builder (StringBuilder. )]
    (doseq [arg args]
      (. builder append (str arg)))
    (. builder toString)))

(defn util-throw-if-item-missing 
  "Throw an exception if the given item is not in the collection.  The args
specify a message to use to notify the rest of the world of the issue."
  [item coll & args]
  (when (not (some (partial = item) coll))
    (throwf (util-stringify args))))

(defonce util-newline-pattern (Pattern/compile "[^\n]+"))

(defn util-split-on-newline 
  "Split the given string on newlines"
  [str]
  (if str
    (re-seq util-newline-pattern str)
    nil))

(defonce util-whitespace-pattern (Pattern/compile "\\S+"))

(defn util-split-on-whitespace 
  "Split the given string on whitespace"
  [str]
  (if str
    (re-seq util-whitespace-pattern str)
    nil))

(defn util-run-cmd 
  "Run the given shell command.  Sets the working directory
to System/getProperty \"user.dir\" if no working directly is
passed in"
  ([cmd-and-args-seq working-dir]
     (let [working-dir (if working-dir
			 working-dir
			 (System/getProperty "user.dir"))]
       (. (Runtime/getRuntime) exec (into-array String cmd-and-args-seq) nil (File. working-dir))))
  ([cmd-and-args]
     (util-run-cmd cmd-and-args nil)))

(defn util-htmlize-string-sequence 
  "Given a sequence of strings, concatenate them, put <br>s at the 
end and surround in <html>...</html>"
  [string-seq]
  (let [builder (StringBuilder.)]
    (. builder append "<html>")
    (let [builder (reduce (fn [builder string] 
			    (when (> (.length builder) 6)
			      (. builder append "<br>"))
			    (. builder append string))
			  builder 
			  string-seq)]
      (. builder toString))))


(defn util-seq-insert
  "Return a seq of the item inserted in a given index.
If the index is < 1, item is inserted at the beginning.  If the
index is > 1, item is appended as last item in sequence"
  [coll item index]
  (let [indexed-col (map vector coll (iterate inc 0))
	[before after] (separate #(< (% 1) index) indexed-col)
	new-col (concat before [[item index]] after)]
    (map first new-col)))

(defn util-seq-remove
  "Return a seq with just like the old seq but
with the nth item removed"
  [coll index]
  (let [indexed-col (map vector coll (iterate inc 0))
	retval (filter #(not(== index (% 1))) indexed-col)]
    (map first retval)))

(defn util-update-map-ref
  "Update a ref map with new entries.  The entries must be
in a sequence (not pairs).  The sequence may be nil"
  [map-ref new-entries]
  (let [entry-seq (seq new-entries)]
    (when entry-seq
      (dosync (ref-set map-ref (apply assoc @map-ref entry-seq))))))
	