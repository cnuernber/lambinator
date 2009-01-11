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
(defstruct loading-system :file-actions)
;maps filename to list of actions to take when in memory.
;these actions happen in an agent thread sent off using
;'send'
(defn create-loading-system
  "Create an empty loading system.  The loading system maps
filenames to items waiting for their load completion"
  []
  (struct loading-system (ref {})))


(defn fs-get-resource-stream
  "Return the resource stream associated with this filename"
  [fname]
  (. (Class/forName "lambinator.fs__init") getResourceAsStream fname))

(defn fs-resource-file-exists? 
  "Determine whether a given resource exists"
  [fname]
  (not (nil? (fs-get-resource-stream fname))))

(defn fs-get-full-path
  "Return the full path if the item is a file.  This function is currently not
correct as it does not return the fully qualified name in the case the 
input name refers to a resource file"
  [fname]
  (if (fs-resource-file-exists? fname)
    fname
    (. (File. fname) getCanonicalPath)))

(defn fs-file-exists?
  "Return true if the file exists on the filesystem"
  [fname]
  (. (File. fname) exists))

(defn fs-file-or-resource-exists? 
  "Return true if the file or the resource exist"
  [fname]
  (or (fs-file-exists? fname)
      (fs-resource-file-exists? fname)))



(defn fs-get-item-input-stream
  "Get an input stream that corresponds to this item.  The stream
could either be a file input stream or a resource input stream"
  [fname]
  (let [rsrc-stream (fs-get-resource-stream fname)]
    (if rsrc-stream
      rsrc-stream
      (FileInputStream. fname))))

;attempts to load a file, producing a byte buffer
;You should have ensured it existed
(defn fs-load-file 
  "Load a file completely into memory and return the bytes"
  [fname]
  (let [file (File. fname)
	input (FileInputStream. file)
	len (. file length)
	bytes (make-array Byte/TYPE len)]
    (. input read bytes)
    bytes))
	

(defn fs-load-resource 
  "Load a resource stream completely into memory and return
the bytes.  This will *really* only work for resource streams"
  [fname]
  (let [stream (fs-get-resource-stream fname)
	length (. stream available)
	retval (make-array Byte/TYPE length)]
    (. stream read retval)
    retval))

(defn fs-load-item 
  "Load an item, whether it is a file or resource.  This function
gives resources priority"
  [fname]
  (if (fs-resource-file-exists? fname)
    (fs-load-resource fname)
    (fs-load-file fname)))

(defn fs-to-hex 
  "Take a byte buffer and return a signed hexadecimal
representation of said buffer"
  [bytes]
  (let [integer (BigInteger. bytes)]
    (. integer toString 16)))
      

(defn fs-md5-hash 
"Take a byte buffer and return an  md5 hash as another byte buffer"
[bytes]
  (let [digest (MessageDigest/getInstance "md5")]
    (. digest update bytes)
    (fs-to-hex (. digest digest))))


(defn- get-filename-actions[actions-map-ref filename]
  (dosync
   (let [actions-map @actions-map-ref
	 actions (actions-map filename)]
     (ref-set actions-map-ref (dissoc actions-map filename))
     actions)))

;Adds an action.  Returns the existing entry before adding new actions
(defn- add-filename-action [actions-map-ref filename action]
  (dosync
   (let [actions @actions-map-ref
	 existing (actions filename)
	 new-actions (assoc actions filename (conj existing action))]
     (ref-set actions-map-ref new-actions)
     existing)))
       

(defn- fs-thread-md5
  "Given a filename and buffer, launch a thread on the CPU thread pool
that creates an md5 hash and then calls the listeners for a given
filename"
  [actions-map-ref filename bytes]
  (let [md5-hash (fs-md5-hash bytes)
	actions (get-filename-actions actions-map-ref filename)
	actions-agent (agent [])]
    (doseq [action actions]
      (send actions-agent (fn [agent] (action bytes filename md5-hash) nil)))))

(defn- fs-thread-load
  "Load a filename on the IO thread pool, then pass file
to CPU thread pool for md5 and final processing"
  [actions-map-ref filename]
  (let [bytes (fs-load-item filename)
	md5-agent (agent [])]
    (send md5-agent 
	  (fn [agent] (fs-thread-md5 actions-map-ref filename bytes) []))))

;task must take 3 arguments:
;byte array, filename, and md5 hash of data
;returns a new loading system and possibly kicked
;off a new loading process.  Returns true if the file
;existed in disk and thus this task happens.
;false if the file didn't exist.
(defn fs-add-load-task
  "Add a given task to be done when a given file is finished loading.
The task should take three arguments, a byte array, a filename, and an 
md5 hash of the file's contents.  This function will probably be
modified such that in the future, you pass in a pair of tasks;
one for if the load is successful and another when there is an
error during load"
  [loading-system fname task]
  (let [full-file-name (fs-get-full-path fname)
	actions-map-ref (loading-system :file-actions)
	exists (fs-file-or-resource-exists? fname)]
     (if exists 
      (let [existing (add-filename-action actions-map-ref full-file-name task)]
	(when (not existing)
	  (let [load-agent (agent {})]
	    (send-off load-agent (fn [agent] (fs-thread-load actions-map-ref full-file-name)))))
	true )
      false)))

(defn fs-get-file-mod-time 
  "Given a file name return the last modified time for that file"
  [fname]
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
(defn- check-file-modifications  
  [file-mod-seq]
  (map (fn [[fname mod-time]]
	 (let [new-mod-time (fs-get-file-mod-time fname)]
	   [fname new-mod-time (not(= mod-time new-mod-time))]))
       file-mod-seq))

;a watcher takes one argument, the filename when the file
;has changed
(defstruct file-mod-watcher-entry :filename :watchers :mod-time)

(defstruct file-mod-watcher-system :file-watchers-ref)
(defn create-file-mod-watcher-system [] (struct file-mod-watcher-system (ref {})))

(defn- internal-add-file-mod-watcher [file-watchers fname watcher-name watcher]
  (let [existing (file-watchers fname)
	existing (if existing
		   existing
		   (struct-map file-mod-watcher-entry
		     :filename fname
		     :watchers {}
		     :mod-time nil))]
    (assoc file-watchers fname 
	   (assoc existing :watchers
		  (assoc (existing :watchers) watcher-name watcher)))))

(defn fs-add-file-mod-watcher 
  "Add a new object to be notified when a particular file changes.  The objects are named
so you can find and remove the watcher if you desire.
The watcher is a function that takes a single argument; the filename to watch"
  [fs-watcher-system fname watcher-name watcher]
  (let [file-watchers-ref (fs-watcher-system :file-watchers-ref)]
    (dosync 
     (alter file-watchers-ref internal-add-file-mod-watcher fname watcher-name watcher))))

(defn- interal-remove-file-mod-watcher 
  [watchers fname watcher-name]
  (let [existing (watchers fname)
	existing-entries (if existing
			   (existing :watchers)
			   nil)
	new-entries (when existing-entries
		      (let [new-entries (dissoc existing-entries watcher-name)]
			(when (seq new-entries)
			  new-entries)))
	new-existing (when new-entries
		       (assoc existing :watchers new-entries))]
    (if new-existing
      (assoc watchers fname new-existing)
      (dissoc watchers fname))))
    

(defn fs-remove-file-mod-watcher 
  "Remove the file modification watcher from the watchers list for a particular file"
  [fs-watcher-system fname watcher-name]
  (let [file-watchers-ref (fs-watcher-system :file-watchers-ref)]
    (dosync 
     (alter file-watchers-ref interal-remove-file-mod-watcher fname watcher-name))))
    

(defn fs-mod-system-check-files 
  "Check registered files and call the relevant watchers with the name
of the file that has changed"
  [fs-watcher-system]
  (let [current-watchers @(fs-watcher-system :file-watchers-ref)
	filemod-seq (map (fn [[fname entry]]
			   [fname (entry :mod-time)])
			 current-watchers)
	filechanges (check-file-modifications filemod-seq)
	truechanges (filter (fn [[- - retval]] retval) filechanges)
	new-entries (map (fn [[fname mod-time -]]
			   [fname (assoc (current-watchers fname) :mod-time mod-time)])
			 truechanges)
	new-entries-expanded (mapcat identity new-entries)]
    (when new-entries
      (doseq [[name new-entry] new-entries]
	(doseq [[watcher-name watcher] (new-entry :watchers)]
	  (try
	   (watcher name)
	   (catch Exception e
	     (. e printStackTrace)))))
      (dosync 
       (alter (fs-watcher-system :file-watchers-ref)
	      (fn [fname-watchers]
		(apply assoc fname-watchers new-entries-expanded)))))))

(defn- split-fname-into-name-ending[fname]
  (if fname
    (let [lindex (. fname lastIndexOf ".")]
      (if (and (>= lindex 0) 
	       (< lindex (. fname length)))
	[(. fname substring (int 0) (int lindex)) (. fname substring (int lindex))]
	[fname ""]))
    ["" ""]))

;returns a java.io.file
(defn fs-get-temp-file 
  "Given a filename (which can be a full path), return a temp file
that bears at least some resemblence to the requested file name"
  [fname]
  (let [temp-dir (System/getProperty "java.io.tmpdir")
	temp-fname (. (File. fname) getName)
	[temp-stem temp-ending] (split-fname-into-name-ending temp-fname)
	current-file (File. temp-dir temp-fname)]
    (if (. current-file exists)
      (first (filter (fn [file]
		       (not (. file exists)))
		     (map (fn [index] (File. temp-dir (util-stringify temp-stem index temp-ending)))
			  (iterate inc 0))))
      current-file)))
    
;http://www.rgagnon.com/javadetails/java-0064.html
(defn fs-copy-stream-to-file 
  "Given an input stream, copy its contents to the given filename"
  [input-stream fname]
  (with-open [output-stream (FileOutputStream. fname)]
    (if (isa? FileInputStream input-stream)
      (let [in-channel (. input-stream getChannel)
	    out-channel (. output-stream getChannel)
					; magic number for Windows, 64Mb - 32Kb)
	    max-count  (- (* 64 1024 1024) (* 32 1024))
	    size (. in-channel size)
	    iterations (take-while (fn [pos]
				     (< pos size))
				   (iterate #(+ (. in-channel transferTo % max-count out-channel) %) 0))]
	(last iterations))
      (let [bytes (make-array Byte/TYPE 1024)
	    read-fn (fn [[bytes-read total-bytes-read]]
		      (let [new-bytes (. input-stream read bytes)
			    valid-read (> new-bytes 0)
			    retval (if valid-read 
				     (do 
				       (. output-stream write bytes 0 new-bytes)
				       (+ total-bytes-read new-bytes))
				     total-bytes-read)]
			[new-bytes retval]))
		      
	    iterations (take-while (fn [[bytes-read total-bytes-read]]
				     (> bytes-read 0))
				   (iterate read-fn (read-fn [0 0])))]
	((last iterations) 1)))))

(defn fs-copy-item-stream-to-file 
  "Given an item that could be either a resource *or* a file, copy its
contents to the given filename"
  [ifname ofname]
  (with-open [input-stream (fs-get-item-input-stream ifname)]
    (fs-copy-stream-to-file input-stream ofname)))
    