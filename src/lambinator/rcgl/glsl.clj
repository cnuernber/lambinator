(ns lambinator.rcgl.glsl
  (:use lambinator.rcgl.util lambinator.log lambinator.fs)
  (:import (javax.media.opengl GL)))

(defn rcgl-glsl-log [log-data-ref type & args]
  (when log-data-ref
    (log-message @log-data-ref "rcgl.glsl:" type args)))

(defstruct rcgl-glsl-item :name :index :size :datatype :datatype-name ) ;either an attribute or a uniform

(def rcgl-glsl-shader-status [:valid :invalid :unused] )
;creates a hash of the shader text so we can identity different shaders
(defstruct rcgl-glsl-shader :gl-handle :gl-log :filename :md5-hash :status :gl-error ) ;either vertex or fragment

(def rcgl-glsl-program-status [:valid :loading :invalid :unused])

(defstruct rcgl-glsl-program :vert-shader :frag-shader 
	   :gl-handle :gl-log :name :uniforms :attributes
	   :status :gl-error ) ;combination of the two

;takes a filename and returns the gl constant that stands for the
;shader type.  This expects functions to end with glslv or glslf
(defn- get-glsl-type-from-fname [fname]
  (if (. fname endsWith "glslv" )
    GL/GL_VERTEX_SHADER
    GL/GL_FRAGMENT_SHADER))


(defn rcglg-shader-valid
  "Returns true if this shader object is valid"
  [shader]
  (if (and shader
	   (= (shader :status) :valid)
	   (> (shader :gl-handle) 0))
    true
    false))

(defn rcglg-program-valid 
  "Return true if this rcgl glsl program is valid"
  [prog]
  (and prog
       (= (prog :status) :valid)
       (> (prog :gl-handle) 0)))

;takes a lambda whos arguments match
;info-fn takes two arguments, an array and an offset
;log-data-fn takes 4 arguments, a length array, an offset
;a byte array and another offset
(defn- get-log-info [info-fn log-data-fn]
  (let [info-args (make-array Integer/TYPE 1)]
    (info-fn info-args 0)
    (let [info-len (aget info-args 0)
	  bytes (make-array Byte/TYPE info-len)
	  log-lengths (make-array Integer/TYPE 1)]
      (aset log-lengths 0 info-len)
      (if (> info-len 0)
	(do
	  (log-data-fn info-len log-lengths 0 bytes 0)
	  (String. bytes 0 (aget log-lengths 0)))
	""))))
      
;this works for program info that returns a single long.
(defn- get-gl-program-int-variable[gl program var-name]
  (rcglu-allocate-gl-item-fn (fn [count args offset] (. gl glGetProgramiv program var-name args offset))))

(defn- get-gl-shader-int-variable[gl shader var-name]
  (rcglu-allocate-gl-item-fn
   (fn [count args offset]
     (. gl glGetShaderiv shader var-name args offset))))
	  

(defn rcglg-create-shader 
  "Create a glsl shader from a given byte array"
  [log-data-ref gl bytes filename md5-hash]
  (rcgl-glsl-log log-data-ref :info "creating shader: " filename)
  (let [type (get-glsl-type-from-fname filename)
	shader (. gl glCreateShader type)
	str-data (String. bytes)
	str-array (make-array String 1)
	len-array (make-array Integer/TYPE 1)]
    (aset str-array 0 str-data)
    (aset len-array 0 (. str-data length))
    (. gl glShaderSource shader 1 str-array len-array 0)
    (. gl glCompileShader shader)
    (let [log-str (get-log-info
		   (fn [info-args offset] (. gl glGetShaderiv shader GL/GL_INFO_LOG_LENGTH info-args offset))
		   (fn [log-len log-lengths len-off log-bytes byte-off] 
		     (. gl glGetShaderInfoLog shader log-len log-lengths len-off log-bytes byte-off)))
	  compile-status (get-gl-shader-int-variable gl shader GL/GL_COMPILE_STATUS)
	  shader-status (if (== compile-status GL/GL_TRUE) :valid :invalid)
	  shader (if (= shader-status :invalid) ;clean up resources for failed compile.
		   (do
		     (. gl glDeleteShader shader)
		     0)
		   shader )]
      (struct-map rcgl-glsl-shader 
	:gl-handle shader 
	:gl-log log-str 
	:filename filename 
	:md5-hash md5-hash 
	:status shader-status 
	:gl-error (rcglu-get-gl-error gl)))))


(defn rcglg-delete-shader
  "Delete an rcgl shader.  Returns a new invalid shader"
  [log-data-ref gl shader]
  (when (rcglg-shader-valid shader)
    (rcgl-glsl-log log-data-ref :info "deleting shader: " (shader :filename))
    (. gl glDeleteShader (shader :gl-handle))
    (assoc shader :gl-handle 0 :status :unused)))


(defn- create-invalid-glsl-program [glslv-filename glslf-filename prog-name]
  (struct-map rcgl-glsl-program 
    :status :invalid
    :vert-shader (struct-map rcgl-glsl-shader :filename glslv-filename :status :invalid)
    :frag-shader (struct-map rcgl-glsl-shader :filename glslf-filename :status :invalid)
    :name prog-name))

(defmacro glsl-get-program-items [gl program active-int max-name-len-int get-mber-fn locate-mber-fn]
  `(let [item-count# (get-gl-program-int-variable ~gl ~program ~active-int)
	 max-name-len# (get-gl-program-int-variable ~gl ~program ~max-name-len-int)
	 name-byte-buf# (make-array Byte/TYPE max-name-len#)
	 len-buf# (make-array Integer/TYPE 1)
	 size-buf# (make-array Integer/TYPE 1)
	 type-buf# (make-array Integer/TYPE 1)]
	;found a hard bug in here.  The map returns a lazy seq.  Thus if you access it
	;from outside the gl thread then you cause a bus error and java completely crashes.
       ;for evaluation of anything in these files lazyiness is definitely not your friend.
     (apply hash-map (doall (mapcat (fn [item#] [(item# :name) item#]) 
				    (map (fn [idx#] 
					   (. ~gl ~get-mber-fn ~program idx# max-name-len# len-buf# 0 size-buf# 0 type-buf# 0 name-byte-buf# 0)
					   (let [name# (String. name-byte-buf# 0 (aget len-buf# 0))]
					   (struct rcgl-glsl-item 
						   name#
						   (. ~gl ~locate-mber-fn ~program name#) (aget size-buf# 0) (aget type-buf# 0)
						   (rcglu-get-opengl-constant-name (aget type-buf# 0)))))
					 (range item-count#)))))))

;returns a program if the underlying shaders were created successfully
(defn rcglg-create-program
  "Create a glslg program from a vertex shader and fragment shader"
  [log-data-ref gl vert-shader frag-shader name]
  (rcgl-glsl-log log-data-ref :info "creating program: " name)
  (let [program (. gl glCreateProgram)]
    (. gl glAttachShader program (vert-shader :gl-handle))
    (. gl glAttachShader program (frag-shader :gl-handle))
    (. gl glLinkProgram program)
    (let [log-str (get-log-info
		   (fn [info-args offset] (. gl glGetProgramiv program GL/GL_INFO_LOG_LENGTH info-args offset))
		   (fn [log-len log-lengths log-ff log-bytes byte-off]
		     (. gl glGetProgramInfoLog program log-len log-lengths log-ff log-bytes byte-off)))
	  link-status (get-gl-program-int-variable gl program GL/GL_LINK_STATUS)
	  retval (struct rcgl-glsl-program vert-shader frag-shader program log-str name [] [] :invalid)]
      ;if it linked successfully then we check out its uniform variables and its attributes
      (if (== link-status GL/GL_TRUE) 
	(let [attributes (glsl-get-program-items 
			  gl 
			  program 
			  GL/GL_ACTIVE_ATTRIBUTES 
			  GL/GL_ACTIVE_ATTRIBUTE_MAX_LENGTH 
			  glGetActiveAttrib 
			  glGetAttribLocation)
	      uniforms (glsl-get-program-items 
			gl 
			program 
			GL/GL_ACTIVE_UNIFORMS 
			GL/GL_ACTIVE_UNIFORM_MAX_LENGTH 
			glGetActiveUniform
			glGetUniformLocation )]
	  (assoc retval :attributes attributes :uniforms uniforms :status :valid :gl-error (rcglu-get-gl-error gl)))
	(do
	  (. gl glDeleteProgram program) ;clean up resources for failed compile
	  (assoc retval :gl-handle 0 :gl-error (rcglu-get-gl-error gl)))))))

(defn rcglg-delete-glsl-program[log-data-ref gl program]
  (when (rcglg-program-valid program)
    (rcgl-glsl-log log-data-ref :info "deleting program: " (program :name))
    (. gl glDeleteProgram (program :gl-handle))
    (assoc program :gl-handle 0 :status :unused :gl-error (rcglu-get-gl-error gl)))) ;note that this says nothing about the shaders

(defmulti set-glsl-uniform (fn [log-data-ref gl uniform-entry var-value] (uniform-entry :datatype)))
;should log that nothing got set.
(defmethod set-glsl-uniform :default [log-data-ref gl uniform-entry var-value] 
  (rcgl-glsl-log log-data-ref :diagnostic "unrecognized uniform type: " (uniform-entry :datatype)))
(defmethod set-glsl-uniform GL/GL_FLOAT [log-data-ref #^GL gl entry var-value]
  (. gl glUniform1f (entry :index) var-value))
;You set the sample to the logical tex unit.
;Example, if you do
;glActiveTexture GL_TEXTURE5
;you set the sampler to 5.  Not GL_TEXTURE5.
(defmethod set-glsl-uniform GL/GL_SAMPLER_2D [log-data-ref #^GL gl entry var-value]
  (. gl glUniform1i (entry :index) var-value))

(defmethod set-glsl-uniform GL/GL_INT [log-data-ref #^GL gl entry var-value]
  (. gl glUniform1i (entry :index) var-value))

(defn rcglg-set-prog-uniforms 
  "Set the rcgl program uniform variables.
- var-pair-seq is a sequence of pairs of name to value.  Based on the
datatype of the value, the system will attempt to set the corresponding
glsl variable"
  [log-data-ref gl var-pair-seq rcgl-glsl-program]
  (let [{ gl-handle :gl-handle uniforms :uniforms } rcgl-glsl-program]
    (doseq [[vname vvalue] var-pair-seq]
      (let [uniform-entry (uniforms vname)]
	(when uniform-entry
	  (set-glsl-uniform log-data-ref gl uniform-entry vvalue))))))



;Below this line is code that is meant to manage the list of programs
;This code has to be callable from user space, offload loading data
;to the fs system threads, and finally do its updating and compiling
;of shaders and programs in the render thread.  Thus it is kind of complex.


;programs maps program name to glsl program
;shaders map shader canonical filepath to glsl shader
;the driver is explicitly supposed to do reference counting
;so you should be able to delete shader objects out from
;under programs with no ill effects, and you should be
;able to delete programs without the underlying shader
;objects being affected.

(defn- update-shaders-with-new-shader [log-data-ref gl shaders-ref new-shader]
  (let [fname (new-shader :filename)
	existing (@shaders-ref fname)]
    (when existing
      (rcglg-delete-shader log-data-ref gl existing))
    (dosync (ref-set shaders-ref (assoc @shaders-ref fname new-shader)))))

;You should replace the program if the update shader is valid
;and its shaders is not valid or the md5 hash doesn't match
(defn- should-replace-program[prog-shader update-shader]
  (and (rcglg-shader-valid update-shader)
       (or (not (rcglg-shader-valid prog-shader))
	   (not (= (prog-shader :md5-hash) (update-shader :md5-hash))))))

(defn- return-valid-shader[prog-shader update-shader]
  (if (rcglg-shader-valid update-shader)
    update-shader
    prog-shader))

(defn- program-should-be-updated[program shaders]
  (let [prog-vs (program :vert-shader)
	prog-fs (program :frag-shader)
	update-vs (shaders (prog-vs :filename))
	update-fs (shaders (prog-fs :filename))]
    (or (should-replace-program prog-vs update-vs)
	(should-replace-program prog-fs update-fs))))

(defn- update-program[log-data-ref gl shaders program]
  (let [prog-vs (program :vert-shader)
	prog-fs (program :frag-shader)
	update-vs (shaders (prog-vs :filename))
	update-fs (shaders (prog-fs :filename))
	name (program :name)
	vs (return-valid-shader prog-vs update-vs)
	fs (return-valid-shader prog-fs update-fs)
	valid-pair (and (rcglg-shader-valid vs)(rcglg-shader-valid fs))
	compile-program (if valid-pair
			  (rcglg-create-program log-data-ref gl vs fs name)
			  program)
	compile-valid (rcglg-program-valid compile-program)
	retval (if compile-valid
		 (do
		   (rcglg-delete-glsl-program log-data-ref gl program) ;if the new program is valid, delete the old program
		   compile-program)
		 program)] ;if the compiled prog isn't valid, return the original
    (when (and valid-pair (not compile-valid))
      (rcgl-glsl-log log-data-ref :diagnostic "Failed to compile program: " name)
      (rcgl-glsl-log log-data-ref :diagnostic (compile-program :gl-log)))
    retval ))

;take the map of programs, and update every one that has a mismatched shader specification
;from the master shaders-ref map
(defn rcglg-update-programs
  "Update the existing rcglg programs with a information.  This will run through
the existing programs and attempt to link them with new shaders.  Should it fail,
it will log a message and keep the existing program."
  [log-data-ref gl programs-ref shaders-ref]
  (let [shaders @shaders-ref 
	programs-that-matter (filter 
			      (fn [[name program]] (program-should-be-updated program shaders))
			      @programs-ref) ;programs that don't need updating don't matter
	;force non-lazy execution as these functions access opengl
	new-programs-list (doall (mapcat
				  (fn [[name program]] [name (update-program log-data-ref gl shaders program)])
				  programs-that-matter))] ;out of the programs that matter, do the update
    (when new-programs-list ;mix the new data into the programs list		
      (dosync (ref-set programs-ref (apply assoc @programs-ref new-programs-list))))))
				     

(defn- finish-shader-load [log-data-ref drawable programs-ref shaders-ref bytes filename md5-hash]
  (let [gl (. drawable getGL)
	existing (@shaders-ref filename)
	md5-matches (if existing
		      (= md5-hash (existing :md5-hash))
		      false)]
    (when (not md5-matches)
      (let [new-shader (rcglg-create-shader log-data-ref gl bytes filename md5-hash)
	    shader-valid (rcglg-shader-valid new-shader)]
	(if shader-valid
	  (do 
	    (update-shaders-with-new-shader log-data-ref gl shaders-ref new-shader)
	    (rcglg-update-programs log-data-ref gl programs-ref shaders-ref))
	  (do
	    (rcgl-glsl-log log-data-ref :diagnostic "Failed to compile shader: " filename)
	    (rcgl-glsl-log log-data-ref :diagnostic (new-shader :gl-log))))))))

;force a shader load of the given filename.
;then this completes, if the md5 hash is different it will rebuild
;all programs that are using this filename as either a vertex or fragment
;shader
(defn rcglg-begin-shader-load [log-data-ref programs-ref shaders-ref loading-system render-tasks-ref filename]
  (let [finish-fn (fn [bytes filename md5-hash] ;when the file is done, add a new render task to load the shader and update
		    (dosync (ref-set            ;any programs that need updating
			     render-tasks-ref 
			     (conj @render-tasks-ref #(finish-shader-load log-data-ref % programs-ref shaders-ref bytes filename md5-hash)))))]
    (fs-add-load-task loading-system filename finish-fn)))

;keyword stands for either :vert-shader or :frag-shader
;This will begin the shader loading if the existing shader isn't valid.
(defn- maybe-begin-shader-load [log-data-ref programs-ref shaders-ref loading-system render-tasks-ref filename existing-shader]
  (when (not (rcglg-shader-valid existing-shader))
    (rcglg-begin-shader-load log-data-ref programs-ref shaders-ref loading-system render-tasks-ref filename)))

;add a new program and return the existing one if it exists.  Must be called from render thread
(defn rcglg-add-new-glsl-program
  "Add a new glsls program, created from the named glslv files"
  [log-data-ref gl programs-ref shaders-ref loading-system render-tasks-ref glslv-filename glslf-filename prog-name]
  (let [existing (@programs-ref prog-name)
	vs (@shaders-ref glslv-filename)
	fs (@shaders-ref glslf-filename)
	matches-exactly (and existing
			     (= ((existing :vert-shader) :filename) glslv-filename)
			     (= ((existing :frag-shader) :filename) glslf-filename))]
    (maybe-begin-shader-load log-data-ref programs-ref shaders-ref loading-system render-tasks-ref glslv-filename vs)
    (maybe-begin-shader-load log-data-ref programs-ref shaders-ref loading-system render-tasks-ref glslf-filename fs)
    
    (when (not matches-exactly)
      (dosync (ref-set programs-ref (assoc @programs-ref prog-name (create-invalid-glsl-program glslv-filename glslf-filename prog-name))))
      (when existing
	(rcglg-delete-glsl-program log-data-ref gl existing)))
    (when (and (rcglg-shader-valid vs) (rcglg-shader-valid fs))
      (rcglg-update-programs log-data-ref gl programs-ref shaders-ref))))

;from a reference to known programs
;filenames should be canonical
;render-tasks are things that should happen *before* the next render pass
;Returns the index of the new program so you can use refer to it later.
;OK to call asynchronously
;do not call this function with files that don't exist.
;I am not certain what will happen, but they will probably silently fail as the loader
;silently ignores files that don't exist.
(defn rcglg-create-program-from-files 
  "Create a new rcglg program.  Creates a function that is run on the render thread"
  [log-data-ref programs-ref shaders-ref loading-system render-tasks-ref glslv-filename glslf-filename prog-name]
  (dosync (ref-set render-tasks-ref 
		   (conj @render-tasks-ref 
			 (fn [drawable]
			   (rcglg-add-new-glsl-program log-data-ref (. drawable getGL) programs-ref shaders-ref loading-system render-tasks-ref 
						       glslv-filename glslf-filename prog-name))))))

;This happens if you change your window resolution or (using GLJpanel) just resize
;the window too far out of what it expects.
;This just does a blocking load of resources until it gets what it wants
;in the render thread.
(defn rcglg-resources-released-reload-all-programs
  "Reload all of the glsl programs from existing sources"
  [log-data-ref drawable programs-ref shaders-ref]
  (let [gl (. drawable getGL)
	new-shaders (doall (mapcat (fn [[name shader]]
				     (let [bytes (fs-load-item name)
					   hash (fs-md5-hash bytes)]
				       [name (rcglg-create-shader log-data-ref gl bytes name hash)]))
				   @shaders-ref))
	new-programs (mapcat (fn [[name prog]]
			       (let [{ { glslv :filename } :vert-shader { glslf :filename } :frag-shader name :name } prog]
				 [name (create-invalid-glsl-program glslv glslf name)]))
			     @programs-ref)]
    (when new-shaders
      (dosync (ref-set shaders-ref (apply assoc @shaders-ref new-shaders))))
    (when new-programs
      (dosync (ref-set programs-ref (apply assoc @programs-ref new-programs))))
    (rcglg-update-programs log-data-ref gl programs-ref shaders-ref)))

(defn- glsl-shader-in-use[programs-ref filename]
  (let [using-progs (filter (fn[[name program]]
				(or (= ((program :vert-shader) :filename) filename)
				    (= ((program :frag-shader) :filename) filename)))
			    @programs-ref)]
    (not (= (first using-progs) nil)))) ;;if there is at least one program using this.

(defn- remove-and-return-item[map-ref key]
  (dosync
   (let [item (@map-ref key)]
     (dosync (ref-set map-ref (dissoc @map-ref key)))
     item )))

(defn- remove-shader-if-not-in-use[programs-ref shaders-ref filename]
  (dosync
   (let [shader (@shaders-ref filename)]
     (if (and shader 
	      (not (glsl-shader-in-use programs-ref filename)))
       (do
	 (dosync (ref-set shaders-ref (dissoc @shaders-ref filename)))
	 shader)
       nil ))))

(defn rcglg-check-rcglg-delete-shader
  "Delete an rcglg shader if not in use"
  [log-data-ref drawable programs-ref shaders-ref filename]
  (let [shader (remove-shader-if-not-in-use programs-ref shaders-ref filename)]
    (when shader
      (rcglg-delete-shader log-data-ref (. drawable getGL) shader))))
    

(defn rcglg-delete-program-and-shaders
  "Delete a glsl program and the corresponding shaders if not in use"
  [log-data-ref drawable programs-ref shaders-ref prog-name]
  (let [program (remove-and-return-item programs-ref prog-name)]
    (when program
      (let [gl (. drawable getGL)
	    glslv ((program :vert-shader) :filename)
	    glslf ((program :frag-shader) :filename)]
	(rcglg-delete-glsl-program log-data-ref gl program)
	(rcglg-check-rcglg-delete-shader log-data-ref drawable programs-ref shaders-ref glslv)
	(rcglg-check-rcglg-delete-shader log-data-ref drawable programs-ref shaders-ref glslf)))))
	    