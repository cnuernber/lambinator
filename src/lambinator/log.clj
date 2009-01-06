(ns lambinator.log
  (:use lambinator.util
	clojure.contrib.seq-utils))

(defstruct log-data :listeners)

(defn create-log-data [] (struct-map log-data))

;a diagnostic message is a message that could contain
;stuff the user *has* to see, like glsl error log info
;info is just info.  Nonsense or nil is decoded to info
(def log-message-types [:diagnostic :info] )

(defmulti decode-log-message-type identity)
(defmethod decode-log-message-type :default [-] :diagnostic)
(defmethod decode-log-message-type :info [-] :info)

;module is rcgl or something like that.
;type is a log type
;the listener needs to take a list of arguments.
(defn log-message [log-data module type & args]
  (if (and log-data args)
    (doseq [listener (log-data :listeners)]
      (try
       (apply listener module type args)
       (catch Exception e
	 (. e printStackTrace))))
    (let [args-str (apply stringify (flatten args))]
      (println module type args-str))))

(defn log-add-listener [log-data listener]
  (assoc log-data :listeners 
	 (conj (log-data :listeners) listener)))