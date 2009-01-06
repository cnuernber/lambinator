(ns lambinator.log
  (:use lambinator.util
	clojure.contrib.seq-utils))

(defstruct log-data :listeners)

(defn create-log-data [] 
  "Create a log data item.  Callers will probably need
to ref the return value"
  (struct-map log-data))

;module is rcgl or something like that.
;type is a log type
;the listener needs to take a list of arguments.
(defn log-message 
  "Send a message to the logging system.  Module and type are
application defined.  Args will all be flattened then
stringified"
  [log-data module type & args]
  (if (and log-data args)
    (doseq [listener (log-data :listeners)]
      (try
       (apply listener module type args)
       (catch Exception e
	 (. e printStackTrace))))
    (let [args-str (apply util-stringify (flatten args))]
      (println module type args-str))))

(defn log-add-listener 
  "Add a listener to the logging system.  Be careful not to do
any blocking IO directly in the listener as it log messages may
be called from time-sensitive areas."
  [log-data listener]
  (assoc log-data :listeners 
	 (conj (log-data :listeners) listener)))