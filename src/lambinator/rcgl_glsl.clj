(in-ns 'lambinator.rcgl)

(defstruct rcgl_glsl_item :name :index :size :datatype :datatype_name ) ;either an attribute or a uniform

(def rcgl_glsl_shader_status [:valid :invalid :unused] )
;creates a hash of the shader text so we can identity different shaders
(defstruct rcgl_glsl_shader :gl_handle :gl_log :filename :md5_hash :status ) ;either vertex or fragment

(def rcgl_glsl_program_status [:valid :loading :invalid :unused])

(defstruct rcgl_glsl_program :vert_shader :frag_shader 
	   :gl_handle :gl_log :name :uniforms :attributes
	   :status ) ;combination of the two

;takes a filename and returns the gl constant that stands for the
;shader type.  This expects functions to end with glslv or glslf
(defn get_glsl_type_from_fname [fname]
  (if (. fname endsWith "glslv" )
    GL/GL_VERTEX_SHADER
    GL/GL_FRAGMENT_SHADER))


(defn glsl_shader_valid[shader]
  (if (and shader
	   (= (shader :status) :valid)
	   (> (shader :gl_handle) 0))
    true
    false))

;;when marking a program as unused we null out everything.
(defn glsl_program_unused [prog]
  (= (prog :status) :unused))

(defn glsl_program_valid [prog]
  (and prog
       (= (prog :status) :valid)
       (> (prog :gl_handle) 0)))

;takes a lambda whos arguments match
;info_fn takes two arguments, an array and an offset
;log_data_fn takes 4 arguments, a length array, an offset
;a byte array and another offset
(defn get_log_info [info_fn log_data_fn]
  (let [info_args (make-array Integer/TYPE 1)]
    (info_fn info_args 0)
    (let [info_len (aget info_args 0)
	  bytes (make-array Byte/TYPE info_len)
	  log_lengths (make-array Integer/TYPE 1)]
      (aset log_lengths 0 info_len)
      (if (> info_len 0)
	(do
	  (log_data_fn info_len log_lengths 0 bytes 0)
	  (String. bytes 0 (aget log_lengths 0)))
	""))))
      
;this works for program info that returns a single long.
(defn get_gl_program_int_variable[gl program var_name]
  (allocate_gl_item
   (fn [count args offset]
     (. gl glGetProgramiv program var_name args offset))))

(defn get_gl_shader_int_variable[gl shader var_name]
  (allocate_gl_item
   (fn [count args offset]
     (. gl glGetShaderiv shader var_name args offset))))
	  

(defn create_glsl_shader [gl bytes filename md5_hash]
  (println "creating shader: " filename)
  (let [type (get_glsl_type_from_fname filename)
	shader (. gl glCreateShader type)
	str_data (String. bytes)
	str_array (make-array String 1)
	len_array (make-array Integer/TYPE 1)]
    (aset str_array 0 str_data)
    (aset len_array 0 (. str_data length))
    (. gl glShaderSource shader 1 str_array len_array 0)
    (. gl glCompileShader shader)
    (let [log_str (get_log_info
		   (fn [info_args offset] (. gl glGetShaderiv shader GL/GL_INFO_LOG_LENGTH info_args offset))
		   (fn [log_len log_lengths len_off log_bytes byte_off] 
		     (. gl glGetShaderInfoLog shader log_len log_lengths len_off log_bytes byte_off)))
	  compile_status (get_gl_shader_int_variable gl shader GL/GL_COMPILE_STATUS)
	  shader_status (if (== compile_status GL/GL_TRUE) :valid :invalid)
	  shader (if (= shader_status :invalid) ;clean up resources for failed compile.
		   (do
		     (. gl glDeleteShader shader)
		     0)
		   shader )]
      (struct rcgl_glsl_shader shader log_str filename md5_hash shader_status))))

(defn delete_glsl_shader[gl shader]
  (when (glsl_shader_valid shader)
    (println "deleting shader: " (shader :filename))
    (. gl glDeleteShader (shader :gl_handle))
    (assoc shader :gl_handle 0 :status :unused)))

;the test fun does all the loading in the gl thread.
;Normally I wouldn't do it like this, as I wouldn't want the
;render thread to be held up loading files of indefinite
;size.
(defn test_create_glsl_shader_from_file [gl filename]
  (let [bytes (fs_load_file filename)
	md5 (fs_md5_hash bytes)]
    (create_glsl_shader gl bytes filename md5)))


(defn create_invalid_glsl_program [glslv_filename glslf_filename prog_name]
  (struct-map rcgl_glsl_program 
    :status :invalid
    :vert_shader (struct-map rcgl_glsl_shader :filename glslv_filename :status :invalid)
    :frag_shader (struct-map rcgl_glsl_shader :filename glslf_filename :status :invalid)
    :name prog_name))

(defmacro glsl_get_program_items [gl program active_int max_name_len_int get_mber_fn]
  (let [item_count (gensym)
	max_name_len (gensym)
	name_byte_buf (gensym)
	len_buf (gensym)
	size_buf (gensym)
	type_buf (gensym)
	idx (gensym)
	item (gensym)]
    `(let [~item_count (get_gl_program_int_variable ~gl ~program ~active_int)
	   ~max_name_len (get_gl_program_int_variable ~gl ~program ~max_name_len_int)
	   ~name_byte_buf (make-array Byte/TYPE ~max_name_len)
	   ~len_buf (make-array Integer/TYPE 1)
	   ~size_buf (make-array Integer/TYPE 1)
	   ~type_buf (make-array Integer/TYPE 1)]
       ;found a hard bug in here.  The map returns a lazy seq.  Thus if you access it
       ;from outside the gl thread then you cause a bus error and java completely crashes.
       ;for evaluation of anything in these files lazyiness is definitely not your friend.
       (apply hash-map (doall (mapcat (fn [~item] [(~item :name) ~item]) 
				      (map (fn [~idx] 
					     (. ~gl ~get_mber_fn ~program ~idx ~max_name_len ~len_buf 0 ~size_buf 0 ~type_buf 0 ~name_byte_buf 0)
					     (struct rcgl_glsl_item 
						     (String. ~name_byte_buf 0 (aget ~len_buf 0)) 
						     ~idx (aget ~size_buf 0) (aget ~type_buf 0)
						     (get_opengl_constant_name (aget ~type_buf 0))))
					   (range ~item_count))))))))

;returns a program if the underlying shaders were created successfully
(defn create_glsl_program[gl vert_shader frag_shader name]
  (println "creating program: " name)
  (let [program (. gl glCreateProgram)]
    (. gl glAttachShader program (vert_shader :gl_handle))
    (. gl glAttachShader program (frag_shader :gl_handle))
    (. gl glLinkProgram program)
    (let [log_str (get_log_info
		   (fn [info_args offset] (. gl glGetProgramiv program GL/GL_INFO_LOG_LENGTH info_args offset))
		   (fn [log_len log_lengths log_ff log_bytes byte_off]
		     (. gl glGetProgramInfoLog program log_len log_lengths log_ff log_bytes byte_off)))
	  link_status (get_gl_program_int_variable gl program GL/GL_LINK_STATUS)
	  retval (struct rcgl_glsl_program vert_shader frag_shader program log_str name [] [] :invalid)]
      ;if it linked successfully then we check out its uniform variables and its attributes
      (if (== link_status GL/GL_TRUE) 
	(let [attributes (glsl_get_program_items gl program GL/GL_ACTIVE_ATTRIBUTES GL/GL_ACTIVE_ATTRIBUTE_MAX_LENGTH glGetActiveAttrib)
	      uniforms (glsl_get_program_items gl program GL/GL_ACTIVE_UNIFORMS GL/GL_ACTIVE_UNIFORM_MAX_LENGTH glGetActiveUniform)]
	  (assoc retval :attributes attributes :uniforms uniforms :status :valid))
	(do
	  (. gl glDeleteProgram program) ;clean up resources for failed compile
	  (assoc retval :gl_handle 0))))))

(defn delete_glsl_program[gl program]
  (when (glsl_program_valid program)
    (println "deleting program: " (program :name))
    (. gl glDeleteProgram (program :gl_handle))
    (assoc program :gl_handle 0 :status :unused))) ;note that this says nothing about the shaders

(defmulti rcgl_set_glsl_uniform (fn [gl uniform_entry var_value] (uniform_entry :datatype)))
;should log that nothing got set.
(defmethod rcgl_set_glsl_uniform :default [&args] )
(defmethod rcgl_set_glsl_uniform GL/GL_FLOAT [#^GL gl entry var_value]
  (. gl glUniform1f (entry :index) var_value))

(defn rcgl_set_glsl_prog_uniforms [gl var_pair_seq rcgl_glsl_program]
  (let [{ gl_handle :gl_handle uniforms :uniforms } rcgl_glsl_program]
    (doseq [[vname vvalue] var_pair_seq]
      (let [uniform_entry (uniforms vname)]
	(when uniform_entry
	  (rcgl_set_glsl_uniform gl uniform_entry vvalue))))))



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

(defn update_shaders_with_new_shader [gl shaders_ref new_shader]
  (let [fname (new_shader :filename)
	existing (@shaders_ref fname)]
    (when existing
      (delete_glsl_shader gl existing))
    (dosync (ref-set shaders_ref (assoc @shaders_ref fname new_shader)))))

;You should replace the program if the update shader is valid
;and its shaders is not valid or the md5 hash doesn't match
(defn should_replace_program[prog_shader update_shader]
  (and (glsl_shader_valid update_shader)
       (or (not (glsl_shader_valid prog_shader))
	   (not (= (prog_shader :md5_hash) (update_shader :md5_hash))))))

(defn return_valid_shader[prog_shader update_shader]
  (if (glsl_shader_valid update_shader)
    update_shader
    prog_shader))

(defn program_should_be_updated[program shaders]
  (let [prog_vs (program :vert_shader)
	prog_fs (program :frag_shader)
	update_vs (shaders (prog_vs :filename))
	update_fs (shaders (prog_fs :filename))]
    (or (should_replace_program prog_vs update_vs)
	(should_replace_program prog_fs update_fs))))

(defn update_program[gl shaders program]
  (let [prog_vs (program :vert_shader)
	prog_fs (program :frag_shader)
	update_vs (shaders (prog_vs :filename))
	update_fs (shaders (prog_fs :filename))
	name (program :name)
	vs (return_valid_shader prog_vs update_vs)
	fs (return_valid_shader prog_fs update_fs)
	valid_pair (and (glsl_shader_valid vs)(glsl_shader_valid fs))
	compile_program (if valid_pair
			  (create_glsl_program gl vs fs name)
			  program)
	compile_valid (glsl_program_valid compile_program)
	retval (if compile_valid
		 (do
		   (delete_glsl_program gl program) ;if the new program is valid, delete the old program
		   compile_program)
		 program)] ;if the compiled prog isn't valid, return the original
    (when (and valid_pair (not compile_valid))
      (println "Failed to compile program: " name)
      (println "gl log: " (compile_program :gl_log)))
    retval ))

;take the map of programs, and update every one that has a mismatched shader specification
;from the master shaders_ref map
(defn update_programs[gl programs_ref shaders_ref]
  (let [shaders @shaders_ref 
	programs_that_matter (filter 
			      (fn [[name program]] (program_should_be_updated program shaders))
			      @programs_ref) ;programs that don't need updating don't matter
	;force non-lazy execution as these functions access opengl
	new_programs_list (doall (mapcat
				  (fn [[name program]] [name (update_program gl shaders program)])
				  programs_that_matter))] ;out of the programs that matter, do the update
    ;mix the new data into the programs list
    (dosync (ref-set programs_ref (apply assoc @programs_ref new_programs_list)))))
				     

(defn finish_shader_load [drawable programs_ref shaders_ref bytes filename md5_hash]
  (let [gl (. drawable getGL)
	existing (@shaders_ref filename)
	md5_matches (if existing
		      (= md5_hash (existing :md5_hash))
		      false)]
    (when (not md5_matches)
      (let [new_shader (create_glsl_shader gl bytes filename md5_hash)
	    shader_valid (glsl_shader_valid new_shader)]
	(if shader_valid
	  (do 
	    (update_shaders_with_new_shader gl shaders_ref new_shader)
	    (update_programs gl programs_ref shaders_ref))
	  (do
	    (println "Failed to compile shader: " filename)
	    (println "gl log: " (new_shader :gl_log))))))))

;force a shader load of the given filename.
;then this completes, if the md5 hash is different it will rebuild
;all programs that are using this filename as either a vertex or fragment
;shader
(defn begin_shader_load [programs_ref shaders_ref loading_system render_tasks_ref filename]
  (let [finish_fn (fn [bytes filename md5_hash] ;when the file is done, add a new render task to load the shader and update
		    (dosync (ref-set            ;any programs that need updating
			     render_tasks_ref 
			     (conj @render_tasks_ref #(finish_shader_load % programs_ref shaders_ref bytes filename md5_hash)))))]
    (fs_add_load_task loading_system filename finish_fn)))

;keyword stands for either :vert_shader or :frag_shader
;This will begin the shader loading if the existing shader isn't valid.
(defn maybe_begin_shader_load [programs_ref shaders_ref loading_system render_tasks_ref filename existing_shader]
  (when (not (glsl_shader_valid existing_shader))
    (begin_shader_load programs_ref shaders_ref loading_system render_tasks_ref filename)))

;add a new program and return the existing one if it exists.  This updates the ref
;OK to call asynchronously
(defn add_new_glsl_program[programs_ref glslv_filename glslf_filename prog_name]
  (let [existing (@programs_ref prog_name)]
    (dosync (ref-set programs_ref (assoc @programs_ref prog_name (create_invalid_glsl_program glslv_filename glslf_filename prog_name))))
    existing ))

;from a reference to known programs
;filenames should be canonical
;render_tasks are things that should happen *before* the next render pass
;Returns the index of the new program so you can use refer to it later.
;OK to call asynchronously
;do not call this function with files that don't exist.
;I am not certain what will happen, but they will probably silently fail as the loader
;silently ignores files that don't exist.
(defn create_glsl_program_from_files [programs_ref shaders_ref loading_system render_tasks_ref glslv_filename glslf_filename prog_name]
  (let [existing_program (add_new_glsl_program programs_ref glslv_filename glslf_filename prog_name)
	vs (@shaders_ref glslv_filename)
	fs (@shaders_ref glslf_filename)]
    (if (and vs fs) ;nothing is nil, we already have these shaders compiled then
      (let [prog_create_task (fn [drawable]
			       (let [gl (. drawable getGL)]
				 (update_programs gl programs_ref shaders_ref)))]
	(dosync (ref-set render_tasks_ref (conj @render_tasks_ref prog_create_task)))) ;add the task to create the object
      (do
	(maybe_begin_shader_load programs_ref loading_system render_tasks_ref glslv_filename vs)
	(maybe_begin_shader_load programs_ref loading_system render_tasks_ref glslf_filename fs)))
    (when existing_program
      (dosync (ref-set render_tasks_ref (conj @render_tasks_ref (fn [drawable]
								  (let [gl (. drawable getGL)]
								    (delete_glsl_program gl existing_program)))))))
    nil))

;This happens if you change your window resolution or (using GLJpanel) just resize
;the window too far out of what it expects.
;This just does a blocking load of resources until it gets what it wants
;in the render thread.
(defn resources_released_reload_all_glsl_programs[drawable programs_ref shaders_ref]
  (let [gl (. drawable getGL)
	new_shaders (doall (mapcat (fn [[name shader]]
				     (let [bytes (fs_load_file name)
					   hash (fs_md5_hash bytes)]
				       [name (create_glsl_shader gl bytes name hash)]))
				   @shaders_ref))
	new_programs (mapcat (fn [[name prog]]
			       (let [{ { glslv :filename } :vert_shader { glslf :filename } :frag_shader name :name } prog]
				 [name (create_invalid_glsl_program glslv glslf name)]))
			     @programs_ref)]
    (dosync (ref-set shaders_ref (apply assoc @shaders_ref new_shaders)))
    (dosync (ref-set programs_ref (apply assoc @programs_ref new_programs)))
    (update_programs gl programs_ref shaders_ref)))


(defstruct rcgl_glsl_manager :programs_ref :shaders_ref)
(defn create_rcgl_glsl_manager [] 
  (struct rcgl_glsl_manager (ref {}) (ref {})))