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