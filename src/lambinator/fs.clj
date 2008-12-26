(ns lambinator.fs
  (:import (java.security MessageDigest)
	   (java.io File FileInputStream)
	   (java.math BigInteger))
  (:use lambinator.util))


;The file system abstraction will completely load files
;in some random thread, then once in memory call another
;threading system to do something interesting with those files.
;there are two thread pools for clojure, one for blocking
;actions and one for cpu intensive actions.  The loading into
;a byte buffer is done in the blocking queue.  Then an md5 hash
;is created in the other threads and the combination of byte[]
;and md5 hash string, along with the canonical filename are all
;passed to a list of objects who will all do something interesting
;with the file.

;actions take a byte array, the filename, the md5 hash of the data
(defstruct loading_system :file_actions)
;maps filename to list of actions to take when in memory.
;these actions happen in an agent thread sent off using
;'send'
(defn create_loading_system[]
  (struct loading_system (ref {})))

;attempts to load a file, producing a byte buffer
;You should have ensured it existed
(defn fs_load_file [fname]
  (let [file (File. fname)
	input (FileInputStream. file)
	len (. file length)
	bytes (make-array Byte/TYPE len)]
    (. input read bytes)
    bytes))

(defn fs_to_hex [bytes]
  (let [integer (BigInteger. bytes)]
    (. integer toString 16)))
      

(defn fs_md5_hash [bytes]
  (let [digest (MessageDigest/getInstance "md5")]
    (. digest update bytes)
    (fs_to_hex (. digest digest))))

;Gets the list of actions and removes the filename
;from the map.
(defn get_filename_actions[actions_map_ref filename]
  (dosync
   (let [actions_map @actions_map_ref
	 actions (actions_map filename)]
     (ref-set actions_map_ref (dissoc actions_map filename))
     actions)))

;Adds an action.  Returns the existing entry before adding new actions
(defn add_filename_action [actions_map_ref filename action]
  (dosync
   (let [actions @actions_map_ref
	 existing (actions filename)
	 new_actions (assoc actions filename (conj existing action))]
     (ref-set actions_map_ref new_actions)
     existing)))
       

(defn fs_thread_md5[actions_map_ref filename bytes]
  ;(println "md5ing in" (lambinator.util/current_thread_id))
  (let [md5_hash (fs_md5_hash bytes)
	actions (get_filename_actions actions_map_ref filename)
	actions_agent (agent [])]
    (doseq [action actions]
      (send actions_agent (fn [agent] (action bytes filename md5_hash) nil)))))

(defn fs_thread_load[actions_map_ref filename]
  ;(println "loading in" (lambinator.util/current_thread_id))
  (let [bytes (fs_load_file filename)
	md5_agent (agent [])]
    (send md5_agent 
	  (fn [agent] (fs_thread_md5 actions_map_ref filename bytes) []))))

;task must take 3 arguments:
;byte array, filename, and md5 hash of data
;returns a new loading system and possibly kicked
;off a new loading process.  Returns true if the file
;existed in disk and thus this task happens.
;false if the file didn't exist.
(defn fs_add_load_task[loading_system fname task]
  (let [file (File. fname)
	actions_map_ref (loading_system :file_actions)]
    (if (. file exists) 
      (let [canonical_name (. file getCanonicalPath)
	    existing (add_filename_action actions_map_ref canonical_name task)]
	(when (not existing)
	  (let [load_agent (agent {})]
	    (send-off load_agent (fn [agent] (fs_thread_load actions_map_ref canonical_name)))))
	true )
      false)))