(ns lambinator.util
  (:import (java.lang.reflect Modifier)))

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
