(ns lambinator.fs
  (:import (java.security MessageDigest)
	   (java.io File FileInputStream FileOutputStream)
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

(defn resource_file_exists? [fname]
  (not (nil? (. (Class/forName "lambinator.fs__init") getResource fname))))

(defn get_full_path[fname]
  (if (resource_file_exists? fname)
    fname
    (. (File. fname) getCanonicalPath)))

(defn file_exists?[fname]
  (. (File. fname) exists))

(defn file_or_resource_exists? [fname]
  (or (file_exists? fname)
      (resource_file_exists? fname)))


(defn fs_get_resource_stream[fname]
  (. (Class/forName "lambinator.fs__init") getResourceAsStream fname))

(defn fs_get_item_input_stream[fname]
  (let [rsrc_stream (fs_get_resource_stream fname)]
    (if rsrc_stream
      rsrc_stream
      (FileInputStream. fname))))

;attempts to load a file, producing a byte buffer
;You should have ensured it existed
(defn fs_load_file [fname]
  (let [file (File. fname)
	input (FileInputStream. file)
	len (. file length)
	bytes (make-array Byte/TYPE len)]
    (. input read bytes)
    bytes))
	

(defn fs_load_resource [fname]
  (let [stream (fs_get_resource_stream fname)
	length (. stream available)
	retval (make-array Byte/TYPE length)]
    (. stream read retval)
    retval))

(defn fs_load_item [fname]
  (if (resource_file_exists? fname)
    (fs_load_resource fname)
    (fs_load_file fname)))

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
  (let [bytes (fs_load_item filename)
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
  (let [full_file_name (get_full_path fname)
	actions_map_ref (loading_system :file_actions)
	exists (file_or_resource_exists? fname)]
    (if exists 
      (let [existing (add_filename_action actions_map_ref full_file_name task)]
	(when (not existing)
	  (let [load_agent (agent {})]
	    (send-off load_agent (fn [agent] (fs_thread_load actions_map_ref full_file_name)))))
	true )
      false)))

(defn fs_get_file_mod_time [fname]
  (let [file (File. fname)]
    (if (. file exists) 
      (. file lastModified)
      nil)))

;takes a list of files tupled to their modification times
;returns a list of files tupled to new modification times and a
;boolean that is true when this one is different than last one.
;no mod time is fine; a new one will be inserted
;and marked as changed.
;a file that doesn't exist gets a nil mod time
(defn check_file_modifications[file_mod_seq]
  (map (fn [[fname mod_time]]
	 (let [new_mod_time (fs_get_file_mod_time fname)]
	   [fname new_mod_time (not(= mod_time new_mod_time))]))
       file_mod_seq))

;a watcher takes one argument, the filename when the file
;has changed
(defstruct file_mod_watcher_entry :filename :watchers :mod_time)

(defstruct file_mod_watcher_system :file_watchers_ref)
(defn create_file_mod_watcher_system [] (struct file_mod_watcher_system (ref {})))

(defn internal_add_file_mod_watcher [file_watchers fname watcher_name watcher]
  (let [existing (file_watchers fname)
	existing (if existing
		   existing
		   (struct-map file_mod_watcher_entry
		     :filename fname
		     :watchers {}
		     :mod_time nil))]
    (assoc file_watchers fname 
	   (assoc existing :watchers
		  (assoc (existing :watchers) watcher_name watcher)))))

(defn fs_add_file_mod_watcher [fs_watcher_system fname watcher_name watcher]
  (let [file_watchers_ref (fs_watcher_system :file_watchers_ref)]
    (dosync 
     (alter file_watchers_ref internal_add_file_mod_watcher fname watcher_name watcher))))

(defn interal_remove_file_mod_watcher [watchers fname watcher_name]
  (let [existing (watchers fname)
	existing_entries (if existing
			   (existing :watchers)
			   nil)
	new_entries (when existing_entries
		      (let [new_entries (dissoc existing_entries watcher_name)]
			(when (seq new_entries)
			  new_entries)))
	new_existing (when new_entries
		       (assoc existing :watchers new_entries))]
    (if new_existing
      (assoc watchers fname new_existing)
      (dissoc watchers fname))))
    

(defn fs_remove_file_mod_watcher [fs_watcher_system fname watcher_name]
  (let [file_watchers_ref (fs_watcher_system :file_watchers_ref)]
    (dosync 
     (alter file_watchers_ref interal_remove_file_mod_watcher fname watcher_name))))
    

(defn fs_mod_system_check_files [fs_watcher_system]
  (let [current_watchers @(fs_watcher_system :file_watchers_ref)
	filemod_seq (map (fn [[fname entry]]
			   [fname (entry :mod_time)])
			 current_watchers)
	filechanges (check_file_modifications filemod_seq)
	truechanges (filter (fn [[_ _ retval]] retval) filechanges)
	new_entries (map (fn [[fname mod_time _]]
			   [fname (assoc (current_watchers fname) :mod_time mod_time)])
			 truechanges)
	new_entries_expanded (mapcat identity new_entries)]
    (when new_entries
      (doseq [[name new_entry] new_entries]
	(doseq [[watcher_name watcher] (new_entry :watchers)]
	  (try
	   (watcher name)
	   (catch Exception e
	     (println (. e getStackTrace))))))
      (dosync 
       (alter (fs_watcher_system :file_watchers_ref)
	      (fn [fname_watchers]
		(apply assoc fname_watchers new_entries_expanded)))))))


;returns a java.io.file
(defn fs_get_temp_file [fname]
  (let [temp_dir (System/getProperty "java.io.tmpdir")
	temp_fname (. (File. fname) getName)]
    (first (filter (fn [file]
		     (not (. file exists)))
		   (map (fn [index] (File. temp_dir (stringify index temp_fname)))
			(iterate inc 0))))))
    
;http://www.rgagnon.com/javadetails/java-0064.html
(defn fs_copy_stream_to_file [input_stream fname]
  (with-open [output_stream (FileOutputStream. fname)]
    (if (isa? FileInputStream input_stream)
      (let [in_channel (. input_stream getChannel)
	    out_channel (. output_stream getChannel)
					; magic number for Windows, 64Mb - 32Kb)
	    max_count  (- (* 64 1024 1024) (* 32 1024))
	    size (. in_channel size)
	    iterations (take-while (fn [pos]
				     (< pos size))
				   (iterate #(+ (. in_channel transferTo % max_count out_channel) %) 0))]
	(last iterations))
      (let [bytes (make-array Byte/TYPE 1024)
	    read_fn (fn [[bytes_read total_bytes_read]]
		      (let [new_bytes (. input_stream read bytes)
			    valid_read (> new_bytes 0)
			    retval (if valid_read 
				     (do 
				       (. output_stream write bytes 0 new_bytes)
				       (+ total_bytes_read new_bytes))
				     total_bytes_read)]
			[new_bytes retval]))
		      
	    iterations (take-while (fn [[bytes_read total_bytes_read]]
				     (> bytes_read 0))
				   (iterate read_fn (read_fn [0 0])))]
	((last iterations) 1)))))

(defn fs_copy_item_stream_to_file [ifname ofname]
  (with-open [input_stream (fs_get_item_input_stream ifname)]
    (fs_copy_stream_to_file input_stream ofname)))
    