(in-ns 'lambinator.rcgl)

;surfaces is a vector of all the known surfaces
;unused is a linked list of the unused surfaces
;render size may be <= surface size
(defstruct context_surface :surface_spec :texture_index :gl_handle :framebuffer_complete )

(defn create_context_surface 
  ( [surface_spec texture_index gl_handle complete]
      (struct context_surface 
	      surface_spec 
	      texture_index 
	      gl_handle
	      complete))
  ( [surface_spec texture_index gl_handle]
      (create_context_surface surface_spec texture_index gl_handle "unknown")))

(defstruct surface_manager :all_surfaces :unused_surfaces)

(defn create_surface_manager [all_surfaces unused_surfaces]
  (struct surface_manager all_surfaces unused_surfaces))


;takes a gl and returns a framebuffer index
(defn allocate_opengl_framebuffer_object [gl]
  (allocate_gl_item (fn [count args offset] (. gl glGenFramebuffersEXT count args offset))))

;takes a gl and a fbo handle, calls release and returns an invalid handle value
(defn release_opengl_framebuffer_object [gl fbo_handle]
  (release_gl_item (fn [count args offset] (. gl glDeleteFramebuffersEXT count args offset)) fbo_handle)
  0)

(defn allocate_opengl_framebuffer_renderbuffer [gl]
  (allocate_gl_item (fn [count args offset] (. gl glGenRenderbuffersEXT count args offset))))

(defn release_opengl_framebffer_renderbuffer [gl rb_hdl]
  (release_gl_item (fn [count args offset] (. gl glDeleteRenderbuffersEXT count args offset)) rb_hdl))



;this is complex.  You want to return the smallest
;surface where both width,height are >= desired
;If you can't get that, you want the one that will
;require the fewest more bytes allocated on the
;card.
;Takes a vector of surfaces, an index of first surface to compare
;an index of the second surface to compare,
;and the desired texture_spec
;This assumes that you have already filtered the possibilities by
;the ones that have matching specification such as format and texture_type
(defn context_surface_matches_better [all_surfaces ctx_surface_idx1 ctx_surface_idx2 desired]
  ;ensure valid surfaces are passed in
  (if (== -1 ctx_surface_idx1)
    ctx_surface_idx2
    (if (== -1 ctx_surface_idx2)
      ctx_surface_idx1
      (let [surface1 (sspec_from_context_surface_index all_surfaces ctx_surface_idx1)
	    surface2 (sspec_from_context_surface_index all_surfaces ctx_surface_idx2)
	    required1 (bytes_required surface1 desired)
	    required2 (bytes_required surface2 desired)]
	(if (and (== required1 0)
		 (== required2 0)) ;desired would fit in either 1 or 2
	  (if (<= (overdraw surface1 desired)
		  (overdraw surface2 desired)) ;return the one with the least overdraw
	    ctx_surface_idx1
	    ctx_surface_idx2)
	  (if (== required1 0) ;desired would fit in s1
	    ctx_surface_idx1
	    (if (== required2 0) ;desired would fit in s2
	      ctx_surface_idx2
	      (if (<= required1 required2) ;return the least number of types required
		ctx_surface_idx1
		ctx_surface_idx2))))))))

(defn filter_possible_context_surfaces [all_surfaces unused_surfaces surface_spec]
  (filter 
   (fn [idx]
     (let [ctx_src (all_surfaces idx)
	   ctx_spec (ctx_src :surface_spec)]
       (and (surface_details_match ctx_spec surface_spec)
	    (> (ctx_src :gl_handle) 0))))
   unused_surfaces))

(defn find_best_context_surface [all_surfaces unused_surfaces surface_spec]
  (let [possible_unused (filter_possible_context_surfaces 
			 all_surfaces unused_surfaces surface_spec)]
    (reduce (fn [best_so_far idx ]
	      (context_surface_matches_better all_surfaces best_so_far 
					      idx surface_spec))
	    -1 
	    possible_unused )))

;return the index of the unused handle a new all_surfaces list
(defn find_empty_context_surface[all_surfaces]
  (util_find_next_matching_index all_surfaces 
				 #(== 0 (% :gl_handle)) (fn [] nil)))


;allocate an FBO but don't allocate the texture; resize it if necessary
(defn internal_allocate_opengl_framebuffer_object [gl texture_index tex_handle, surface_spec]
  (let [texture_spec (surface_spec :texture_spec)
	fbo_handle (allocate_opengl_framebuffer_object gl)
	internal_format (gl_internal_format_from_texture_spec texture_spec)
	external_format (gl_external_format_from_texture_spec texture_spec)
	external_datatype (gl_external_datatype_from_texture_spec texture_spec) 
	[width height] (texture_spec :size)
	depth_bits (surface_spec :depth_bits)]
    ;game on
    (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT fbo_handle)
    (. gl glBindTexture GL/GL_TEXTURE_2D tex_handle)
    ;allocate space on the card...
    (. gl glTexImage2D GL/GL_TEXTURE_2D 0 internal_format width height 0 external_format external_datatype nil)
    (. gl glFramebufferTexture2DEXT 
       GL/GL_FRAMEBUFFER_EXT GL/GL_COLOR_ATTACHMENT0_EXT GL/GL_TEXTURE_2D 
       tex_handle 0)
    (when (not (= :none depth_bits))
      (let [depth_constant (convert_depth_bits_to_gl_constant depth_bits)
	    depth_buffer (allocate_opengl_framebuffer_renderbuffer gl)]
	(. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT depth_buffer)
	(. gl glRenderbufferStorageEXT GL/GL_RENDERBUFFER_EXT depth_constant width height)
	(. gl glFramebufferRenderbufferEXT 
	   GL/GL_FRAMEBUFFER_EXT GL/GL_DEPTH_ATTACHMENT_EXT GL/GL_RENDERBUFFER_EXT depth_buffer)))
    (let [bufComplete (. gl glCheckFramebufferStatusEXT GL/GL_FRAMEBUFFER_EXT)
	  bufCompleteName (get_opengl_constant_name bufComplete)]
    ;omitting status check for now until I figure some sequence of formats I can
    ;run through
      (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT 0)
      (create_context_surface surface_spec texture_index fbo_handle bufCompleteName))))

;Releases opengl resources related to the context surface
;sets gl handle to an invalid value.
(defn internal_release_context_surface [gl context_surface]
  (let [old_handle (context_surface :gl_handle)
	invalid_gl_handle (release_opengl_framebuffer_object gl old_handle)]
    (assoc context_surface :gl_handle invalid_gl_handle)))

;release the context surface.
;returns the new list of all surfaces and the new textures list
(defn release_context_surface[gl index all_surfaces textures]
  (let [context_surface (all_surfaces index)
	old_texture_index (context_surface :texture_index)
	new_textures (release_context_texture_item gl textures old_texture_index)
	new_surface (internal_release_context_surface gl context_surface)]
    [(assoc all_surfaces index new_surface) textures]))

 
;returns a new context_surface and a new context_textures_list
;as it updates the context texture to reflect the new width and height
;[context_surface context_textures_list]
(defn reallocate_opengl_framebuffer_object [gl context_surface surface_spec context_textures_list]
  (let [context_texture_index (context_surface :texture_index) ;which texture is this context using
	context_texture (context_textures_list context_texture_index) ;get that texture
	tex_handle (context_texture :gl_handle) ;get its texture handle
	context_surface (internal_release_context_surface gl context_surface) ;create new invalid contex surface
	context_surface_spec (context_surface :surface_spec) ;get old surface spec
	context_texture_spec (context_surface_spec :texture_spec) ;get old texture spec
	surface_texture_spec (surface_spec :texture_spec) ;get new texture spec
	[cw ch] (context_texture_spec :size) ;get current sizes
	[dw dh] (surface_texture_spec :size) ;get desired sizes
	newWidth (max cw dw) ;get new sizes
	newHeight (max ch dh)
	;create a new texture spec with the new width and height
	new_texture_spec (assoc context_texture_spec :size [newWidth newHeight])

	;create a new surface description with the updated texture specifications
	new_surface_spec (assoc surface_spec :texture_spec new_texture_spec)

	;create a new context_texture with the new texture description that reflects
	;that we are going to change the size of its data
	new_context_texture (assoc context_texture :gl_handle tex_handle :texture_spec new_texture_spec)

	;update the textures list
	new_context_textures_list (assoc context_textures_list context_texture_index new_context_texture)

	;create a new context surface with the new information, this allocates the actual data on card
	new_context_surface (internal_allocate_opengl_framebuffer_object gl context_texture_index tex_handle new_surface_spec )]
    [new_context_surface new_context_textures_list]))

;reallocator is a function that takes a context surface spec
;and returns a new one that is big enough to fit surface spec
;into.
(defn reallocate_context_surface_if_necessary [gl index all_surfaces surface_spec context_textures_list]
  (let [ctx_src (all_surfaces index)
	ctx_spec (ctx_src :surface_spec)
	bytes_required (bytes_required ctx_spec surface_spec)]
    (if ( > bytes_required 0 )
      (let [[new_surface new_textures_list] (reallocate_opengl_framebuffer_object gl ctx_src surface_spec context_textures_list)
	    new_all_surfaces (assoc all_surfaces index new_surface)]
	[new_all_surfaces new_textures_list])
	[all_surfaces context_textures_list])))

(defn create_new_context_surface [gl surface_manager context_textures_vector surface_spec]
  (let [all_surfaces (surface_manager :all_surfaces)
	unused_surfaces (surface_manager :unused_surfaces)
	[retval new_all_surfaces] (find_empty_context_surface all_surfaces)
	[new_context_textures_list texture_index] (allocate_context_texture_item gl context_textures_vector (surface_spec :texture_spec))
	new_context_texture (new_context_textures_list texture_index)
	tex_handle (new_context_texture :gl_handle)
	new_context_surface (internal_allocate_opengl_framebuffer_object gl texture_index tex_handle surface_spec)
	new_all_surfaces (assoc new_all_surfaces retval new_context_surface)
	new_surface_manager (assoc surface_manager :all_surfaces new_all_surfaces)]
    [new_surface_manager new_context_textures_list retval]))

(defn reuse_existing_context_surface [gl surface_manager context_textures_vector surface_spec index]
  (let [all_surfaces (surface_manager :all_surfaces)
	unused_surfaces (surface_manager :unused_surfaces)
	[new_all_surfaces new_context_textures_list] (reallocate_context_surface_if_necessary 
						      gl index all_surfaces surface_spec
						      context_textures_vector)
	new_unused_surfaces (filter #(not (== index %)) unused_surfaces)
	new_surface_manager (assoc surface_manager :all_surfaces new_all_surfaces :unused_surfaces new_unused_surfaces)]
    [new_surface_manager new_context_textures_list index]))

;Allocator must return a context surface if requested
;returns a list of new surface manager along with new context surface index
;re-allocator must take the surface spec, the list of all surfaces, and an index
;that *may* need to be re-allocated if its width or height is less than
;specified in the surface spec. IT should return a new list of all surfaces
;or the current list if no allocation is desired.
(defn allocate_context_surface [gl surface_manager context_textures_vector surface_spec ]
  (let [all_surfaces (surface_manager :all_surfaces)
	unused_surfaces (surface_manager :unused_surfaces)
	best_fn #(find_best_context_surface all_surfaces unused_surfaces surface_spec)
	best_valid_fn #(not(== % -1))
	create_new_fn (fn [idx] (create_new_context_surface gl surface_manager context_textures_vector surface_spec))
	reuse_existing_fn #(reuse_existing_context_surface gl surface_manager context_textures_vector surface_spec %)]
    (find_best_match_or_create_new best_fn best_valid_fn create_new_fn reuse_existing_fn)))

;Returns the index to the surface manager's unused list.
;Returns a new surface manager
(defn return_surface [surface_manager index]
  (assoc surface_manager :unused_surfaces (conj (surface_manager :unused_surfaces) index)))