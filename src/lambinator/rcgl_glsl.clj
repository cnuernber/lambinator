(in-ns 'lambinator.rcgl)

(defstruct rcgl_glsl_item :name :index :size :datatype ) ;either an attribute or a uniform

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
    (let [info_len (aget info_fn 0)
	  bytes (make-array Byte/TYPE info_len)
	  log_lengths (make-array Integer/TYPE 1)]
      (aset log_lengths 0 info_len)
      (log_data_fn log_lengths 0 bytes 0)
      (String. bytes))))
      
;this works for program info that returns a single long.
(defn get_gl_program_int_variable[gl program var_name]
  (allocate_gl_item
   (fn [count args offset]
     (. gl glGetProgramiv program var_name args offset))))
	  

(defn create_glsl_script [gl type bytes filename md5_hash]
  (let [shader (. gl glCreateShader type)
	str_data (String. bytes)]
    (. gl glShaderSource shader 1 str_data nil)
    (. gl glCompileShader shader)
    (let [log_str (get_log_info
		   (fn [info_args offset] (. gl glGetShaderiv shader GL/GL_INFO_LOG_LENGTH info_args offset))
		   (fn [log_lengths len_off log_bytes byte_off] 
		     (. gl glGetShaderInfoLog shader log_lengths len_off log_bytes byte_off)))]
      (struct rcgl_glsl_script shader log_str filename md5_hash))))

;returns a program if the underlying scripts were created successfully
(defn create_glsl_program[gl vert_script frag_script name]
  (let [program (. gl glCreateProgram)]
    (. gl glAttachShader program (vert_script :gl_handle))
    (. gl glAttachShader program (frag_script :gl_handle))
    (. gl glLinkProgram program)
    (let [log_str (get_log_info
		   (fn [info_args offset] (. gl glGetProgramiv program GL/GL_INFO_LOG_LENGTH info_args offset))
		   (fn [log_lengths log_ff log_bytes byte_off]
		     (. gl glGetProgramInfoLog program log_lengths log_ff log_bytes byte_off)))
	  link_status (get_gl_program_int_variable gl program GL/GL_LINK_STATUS)
	  retval (struct rcgl_glsl_program vert_script frag_script program log_str name [] [])]
      ;if it linked successfully then we check out its uniform variables and its attributes
      (if (== link_status GL/GL_TRUE) 
	(let [att_count (get_gl_program_int_variable gl program GL/GL_ACTIVE_ATTRIBUTES)
	      uniform_count (get_gl_program_int_variable gl program GL/GL_ACTIVE_UNIFORMS)
	      att_name_len (get_gl_program_int_variable gl program GL/GL_ACTIVE_ATTRIBUTE_MAX_LENGTH)
	      uni_name_len (get_gl_program_int_variable gl program GL/GL_ACTIVE_UNIFORM_MAX_LENGTH)
	      max_name_len (max att_name_len uni_name_len)
	      name_byte_buf (make-array Byte/TYPE max_name_len)
	      len_buf (make-array Integer/TYPE 1)
	      size_buf (make-array Integer/TYPE 1)
	      type_buf (make-array Integer/TYPE 1)
	      attributes (map (fn [idx] 
				(. gl glGetActiveAttrib program idx max_name_len len_buf size_buf type_buf name_byte_buf)
				(struct rcgl_glsl_item 
					(String. name_byte_buf 0 (aget len_buf 0)) 
					idx (aget size_buf 0) (aget type_buf 0)))
			      (range att_count))
	      uniforms (map (fn [idx]
			      (. gl glGetActiveUniform program idx max_name_len len_buf size_buf type_buf name_byte_buf)
			      (struct rcgl_glsl_item
				      (String. name_byte_buf 0 (aget len_buf 0)) 
				      idx (aget size_buf 0) (aget type_buf 0)))
			    (range att_count))]
	  (assoc retval :attributes attributes :uniforms uniforms))
	retval))))