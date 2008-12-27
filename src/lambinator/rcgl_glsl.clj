(in-ns 'lambinator.rcgl)

(defstruct rcgl_glsl_item :name :index :size :datatype :datatype_name ) ;either an attribute or a uniform

(def rcgl_glsl_shader_status [:valid :invalid :unused] )
;creates a hash of the shader text so we can identity different shaders
(defstruct rcgl_glsl_shader :gl_handle :gl_log :filename :md5_hash :status ) ;either vertex or fragment

(def rcgl_glsl_program_status [:valid :loading :invalid :unused])

(defstruct rcgl_glsl_program :vert_shader :frag_shader 
	   :gl_handle :gl_log :name :uniforms :attributes
	   :status ) ;combination of the two

(defstruct rcgl_glsl_manager :programs)

;takes a filename and returns the gl constant that stands for the
;shader type.  This expects functions to end with glslv or glslf
(defn get_glsl_type_from_fname [fname]
  (if (. fname endsWith "glslv" )
    GL/GL_VERTEX_SHADER
    GL/GL_FRAGMENT_SHADER))


(defn glsl_shader_valid[shader]
  (if (and shader
	   (> (shader :gl_handle) 0)
	   (= (shader :status) :valid))
    true
    false))

;;when marking a program as unused we null out everything.
(defn glsl_program_unused [prog]
  (= (prog :status) :unused))

(defn glsl_program_valid [prog]
  (= (prog :status) :valid))

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
	  ]
      (struct rcgl_glsl_shader shader log_str filename md5_hash shader_status))))

(defn delete_glsl_shader[gl shader]
  (. gl glDeleteShader (shader :gl_handle))
  (assoc shader :gl_handle 0 :status :unused))

;the test fun does all the loading in the gl thread.
;Normally I wouldn't do it like this, as I wouldn't want the
;render thread to be held up loading files of indefinite
;size.
(defn test_create_glsl_shader_from_file [gl filename]
  (let [bytes (fs_load_file filename)
	md5 (fs_md5_hash bytes)]
    (create_glsl_shader gl bytes filename md5)))

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
	retval))))

(defn delete_glsl_program[gl program]
  (. gl glDeleteProgram (program :gl_handle))
  (assoc program :gl_handle 0 :status :unused)) ;note that this says nothing about the shaders


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

	
;the new shader had better be a valid new shader.

;Returns the set of programs that have a shader with this filename
(defn find_glsl_indexed_programs_by_filename [indexed_programs keyword filename]
  (filter (fn [[index program]]
	    (let [shader (program keyword)]
	      (= (shader :filename) filename)))
	  indexed_programs))

;returns the indexed programs filtered by the ones
;that have a matching shader by filename and md5
;returns both the matching and the non-matching sequence
;in a vector.
(defn find_glsl_indexed_programs_by_filename_md5 [indexed_programs keyword filename md5]
  (separate (fn [[index program]]
	    (let [shader (program keyword)]
	      (and (= (shader :filename) filename)
		   (= (shader :md5_hash) md5)
		   (= (shader :status) :valid))))
	    indexed_programs))


;Update the program if the update is valid.
;There are several possibilities.
;First, the update may not be valid.  In this case there is
;no change to the existing program.
;Next the update may be valid but the program's *other* shader may not be
;In this case, we just update the program with the new shader.
;Finally both update and other shader may be valid.  In this case
;we attempt to create a new program and output a message if
;unable to do so.
(defn update_rcgl_glsl_program [gl program keyword released_shader_map update_shader]
  (let [prog_key (program :gl_handle)
	prog_name (program :name)
	is_update_vert (= keyword :vert_shader)
	update_valid (glsl_shader_valid update_shader)
	other_program (if is_update_vert
			(program :frag_shader)
			(program :vert_shader))]
    (if update_valid
      (let [shader (program keyword) ;old shader from program
	    [vs fs] (if is_update_vert
		      [update_shader (program :frag_shader)]
		      [(program :vert_shader) update_shader]) ;get correct vs,fs mapping pair
	    program_valid (glsl_program_valid program)
	    other_valid (glsl_shader_valid other_program)
	    program (if program_valid 
		      (delete_glsl_program gl program) ;get new released program
		      program)
	    release_key (shader :gl_handle) ;release old shader
	    is_released (= (released_shader_map release_key) nil)
	    shader (if (and (glsl_shader_valid shader) ;;if valid and not released, release it
			    (not is_released))
		     (delete_glsl_shader gl shader)
		     shader)
	    released_shader_map (assoc released_shader_map release_key shader);ensure we don't release same shader twice
	    new_program (if other_valid ;create a new program if we have both a valid vs and fs
			  (create_glsl_program gl vs fs prog_name)
			  (assoc program keyword update_shader))
	    new_program_valid (glsl_program_valid new_program)]
					;else just add the update shader to the program def
	(when (and other_valid update_valid (not new_program_valid))
	  (println "Failed attempt at creating program: " (program :name))
	  (println "gl-log: " (program :gl_log)))
	[new_program released_shader_map]) ;;return a new program and a new shader map
      [program released_shader_map])))
	
;ensure an existing shader doesn't meet these exact specifications...
;meaning its filename and md5_hash match.  If this is the case, just attempt
;to update existing programs that may not have the latest program.  If it
;is not the case.  Due the to vagaries of threaded file loading, we can't
;always be 100% correct but in the worse case we should be won't be all
;that inefficient.
(defn finish_shader_load [drawable programs_ref keyword bytes filename md5_hash]
  (let [gl (. drawable getGL)
	type (get_glsl_type_from_fname filename)
	indexed_programs (map vector (iterate inc 0) @programs_ref)
	possible_updates (find_glsl_indexed_programs_by_filename indexed_programs keyword filename)
	[existing_matches need_updating] (find_glsl_indexed_programs_by_filename_md5 indexed_programs keyword filename md5_hash)
	first_existing (first existing_matches)
	update_shader (if first_existing
			(first_existing keyword)
			(create_glsl_shader gl bytes filename md5_hash))]
    (when (not (glsl_shader_valid update_shader))
      (do
	(println "Invalid shader from file: " (update_shader :filename))
	(println "gl log: " (update_shader :gl_log))))
    (let [[updated_programs released_shader_map] (reduce 
						  (fn [[new_updated_programs released_shader_map] [index program]]
						    (let [[new_program new_map] (update_rcgl_glsl_program 
										 gl 
										 program keyword 
										 released_shader_map 
										 update_shader)
							  new_updated_programs (conj new_updated_programs index)]
						      [(conj new_updated_programs new_program) new_map]))
						  [[] {}]
						  need_updating )
	  updated_programs (doall updated_programs)] ;force evaluation right away
    (dosync (ref-set programs_ref (apply assoc @programs_ref updated_programs))))))


;keyword stands for either :vert_shader or :frag_shader
;This will begin the shader loading if the existing shader isn't valid.
(defn maybe_begin_shader_load [programs_ref loading_system render_tasks_ref keyword filename existing_shader]
  (when (not (glsl_shader_valid existing_shader))
    (let [finish_fn (fn [bytes filename md5_hash] ;when the file is done, add a new render task to load the shader and update
		      (dosync (ref-set            ;any programs that need updating
			       render_tasks_ref 
			       (conj render_tasks_ref #(finish_shader_load % programs_ref keyword bytes filename md5_hash)))))]
      (fs_add_load_task loading_system filename finish_fn))))
		    
    

(defn next_unused_glsl_program_index [programs glslv_filename glslf_filename prog_name]
  (util_find_next_matching_index 
   programs
   glsl_program_unused
   (fn [] 
     (struct-map rcgl_glsl_program 
       :status :invalid
       :vert_shader (struct-map rcgl_glsl_shader :filename glslv_filename :status :invalid)
       :frag_shader (struct-map rcgl_glsl_shader :filename glslf_filename :status :invalid)
       :name prog_name))))

(defn find_matching_valid_glsl_shader [programs keyword filename]
  (let [shaders (map #(% keyword) programs)]
    (first (filter #(and (= (% :filename) filename) ;filenames match and is valid
			 (glsl_shader_valid %))))))

(defn create_loading_program_stub [programs_ref glslv_filename glslf_filename prog_name]
  (dosync
   (let [[idx programs] (next_unused_glsl_program_index @programs_ref glslv_filename glslf_filename prog_name)]
     (ref-set programs_ref programs)
     [idx programs])))

;from a reference to known programs
;filenames should be canonical
;render_tasks are things that should happen *before* the next render pass
;Returns the index of the new program so you can use refer to it later.
(defn rcgl_glsl_create_program [programs_ref loading_system render_tasks_ref glslv_filename glslf_filename prog_name]
   (let [[idx programs] (create_loading_program_stub programs_ref glslv_filename glslf_filename prog_name)
	 vs (find_matching_valid_glsl_shader :vert_shader glslv_filename)
	 fs (find_matching_valid_glsl_shader :frag_shader glslf_filename)]
     (if (and vs fs) ;nothing is nil, we already have these shaders compiled then
       (let [prog_create_task (fn [drawable]
				(let [gl (. drawable getGL)]
				  (dosync 
				   (ref-set programs_ref 
					    (assoc @programs_ref idx (create_glsl_program gl vs fs prog_name))))))]
	 (dosync (ref-set render_tasks_ref (conj @render_tasks_ref prog_create_task)))) ;add the task to create the object
       (do
	 (maybe_begin_shader_load programs_ref loading_system render_tasks_ref :vert_shader glslv_filename vs)
	 (maybe_begin_shader_load programs_ref loading_system render_tasks_ref :frag_shader glslf_filename fs)))
     idx))