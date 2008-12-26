(in-ns 'lambinator.rcgl)

(defstruct rcgl_glsl_item :name :index :size :datatype :datatype_name ) ;either an attribute or a uniform

;creates a hash of the shader text so we can identity different shaders
(defstruct rcgl_glsl_script :gl_handle :gl_log :filename :md5_hash ) ;either vertex or fragment

(defstruct rcgl_glsl_program :vert_script :frag_script 
	   :gl_handle :gl_log :name :uniforms :attributes ) ;combination of the two

(defstruct rcgl_glsl_manager :programs)

;takes a filename and returns the gl constant that stands for the
;shader type.  This expects functions to end with glslv or glslf
(defn get_glsl_type_from_fname [fname]
  (if (. fname endsWith "glslv" )
    GL/GL_VERTEX_SHADER
    GL/GL_FRAGMENT_SHADER))

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
	  

(defn create_glsl_script [gl type bytes filename md5_hash]
  (let [shader (. gl glCreateShader type)
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
		     (. gl glGetShaderInfoLog shader log_len log_lengths len_off log_bytes byte_off)))]
      (struct rcgl_glsl_script shader log_str filename md5_hash))))

;the test fun does all the loading in the gl thread.
;Normally I wouldn't do it like this, as I wouldn't want the
;render thread to be held up loading files of indefinite
;size.
(defn test_create_glsl_script_from_file [gl filename]
  (let [type (get_glsl_type_from_fname filename)
	bytes (fs_load_file filename)
	md5 (fs_md5_hash bytes)]
    (create_glsl_script gl type bytes filename md5)))

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
 	    
  

;returns a program if the underlying scripts were created successfully
(defn create_glsl_program[gl vert_script frag_script name]
  (let [program (. gl glCreateProgram)]
    (. gl glAttachShader program (vert_script :gl_handle))
    (. gl glAttachShader program (frag_script :gl_handle))
    (. gl glLinkProgram program)
    (let [log_str (get_log_info
		   (fn [info_args offset] (. gl glGetProgramiv program GL/GL_INFO_LOG_LENGTH info_args offset))
		   (fn [log_len log_lengths log_ff log_bytes byte_off]
		     (. gl glGetProgramInfoLog program log_len log_lengths log_ff log_bytes byte_off)))
	  link_status (get_gl_program_int_variable gl program GL/GL_LINK_STATUS)
	  retval (struct rcgl_glsl_program vert_script frag_script program log_str name [] [])]
      ;if it linked successfully then we check out its uniform variables and its attributes
      (if (== link_status GL/GL_TRUE) 
	(let [attributes (glsl_get_program_items gl program GL/GL_ACTIVE_ATTRIBUTES GL/GL_ACTIVE_ATTRIBUTE_MAX_LENGTH glGetActiveAttrib)
	      uniforms (glsl_get_program_items gl program GL/GL_ACTIVE_UNIFORMS GL/GL_ACTIVE_UNIFORM_MAX_LENGTH glGetActiveUniform)]
	  (assoc retval :attributes attributes :uniforms uniforms))
	retval))))

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
	  