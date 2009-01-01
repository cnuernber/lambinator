(ns lambinator.log
  (:use lambinator.util
	clojure.contrib.seq-utils))

(defstruct log_data :listeners)

(defn create_log_data [] (struct-map log_data))

;a diagnostic message is a message that could contain
;stuff the user *has* to see, like glsl error log info
;info is just info.  Nonsense or nil is decoded to info
(def log_message_types [:diagnostic :info] )

(defmulti decode_log_message_type identity)
(defmethod decode_log_message_type :default [_] :diagnostic)
(defmethod decode_log_message_type :info [_] :info)

;module is rcgl or something like that.
;type is a log type
;the listener needs to take a list of arguments.
(defn log_message [log_data module type & args]
  (if (and log_data args)
    (doseq [listener (log_data :listeners)]
      (try
       (apply listener module type args)
       (catch Exception e
	 (println (. e getStackTrace)))))
    (let [args_str (apply stringify (flatten args))]
      (println module type args_str))))

(defn log_add_listener [log_data listener]
  (assoc log_data :listeners 
	 (conj (log_data :listeners) listener)))