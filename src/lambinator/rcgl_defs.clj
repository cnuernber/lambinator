(in-ns 'lambinator.rcgl)
;Simple render context implementation that binds the
;render commands to sets of opengl calls

(defn create_context_texture [texture_spec gl_handle]
  (struct context_texture texture_spec gl_handle))

(defn create_texture_manager [textures]
  (struct texture_manager textures))

(defn create_context_surface [surface_spec texture_index gl_handle]
  (struct context_surface 
	  surface_spec 
	  texture_index 
	  gl_handle))

(defn create_surface_manager [all_surfaces unused_surfaces]
  (struct surface_manager all_surfaces unused_surfaces))

(defn create_render_context [surface_manager]
  (struct render_context surface_manager))

(defn sspec_from_context_surface_index [all_surfaces idx]
  ((all_surfaces idx) :surface_spec))

(defn get_opengl_constant_name [constant]
  (let [field (first (find_static_fields_by_value "javax.media.opengl.GL" constant))]
    (if field
      (. field getName)
      "")))

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
  (let [indexed_items (map list all_surfaces (iterate inc 0))
	filtered_items (filter #(== 0 ((first %) :gl_handles)) indexed_items)
	unused (first filtered_items)]
    (if unused
      [(second unused) all_surfaces]
      (let [retval (count all_surfaces) 
	    new_all_surfaces (conj all_surfaces nil)]
	  [retval new_all_surfaces]))))

;A lot of gl calls have the form of
;fn( int number, array ret_data, int offset )
;this wraps creating the retvals and such
(defn allocate_gl_item[lmbda]
  (let [args (make-array Integer/TYPE 1)]
    (lmbda 1 args 0)
    (aget args 0)))

(defn release_gl_item[lmbda handle]
  (let [args (make-array Integer/TYPE 1)]
    (aset-int args 0 handle)
    (lmbda 1 args 0)))
	
;takes a gl, a texture spec,
;and returns a context_texture
;This simply allocates a texture handle
(defn allocate_opengl_texture [gl texture_spec]
  (create_context_texture 
   texture_spec 
   (allocate_gl_item (fn [count args offset] (. gl glGenTextures count args offset)))))

;releases the gl handle and returns a new context texture
;with the handle value set to -1
(defn release_opengl_texture [gl context_texture]
  (let [tex_handle (context_texture :gl_handle)]
    (release_gl_item (fn [count args offset] (. gl glDeleteTextures count args offset)) tex_handle)
    (create_context_texture (context_texture :texture_spec) -1)))

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

(defmulti gl_internal_format_from_texture_format_and_type #(vector %1 %2))
(defmethod gl_internal_format_from_texture_format_and_type :default [arg1 arg2] GL/GL_RGBA8 )

(defmethod gl_internal_format_from_texture_format_and_type [:rgba :ubyte] [arg1 arg2] GL/GL_RGBA8 )
(defmethod gl_internal_format_from_texture_format_and_type [:rgba :short] [arg1 arg2] GL/GL_RGBA16 )
(defmethod gl_internal_format_from_texture_format_and_type [:rgba :half_float] [arg1 arg2] GL/GL_FLOAT_RGBA16_NV )
(defmethod gl_internal_format_from_texture_format_and_type [:rgba :float] [arg1 arg2] GL/GL_FLOAT_RGBA32_NV )

(defmethod gl_internal_format_from_texture_format_and_type [:rgb :ubyte] [arg1 arg2] GL/GL_RGB8 )
(defmethod gl_internal_format_from_texture_format_and_type [:rgb :short] [arg1 arg2] GL/GL_RGB16 )
(defmethod gl_internal_format_from_texture_format_and_type [:rgb :half_float] [arg1 arg2] GL/GL_FLOAT_RGB16_NV )
(defmethod gl_internal_format_from_texture_format_and_type [:rgb :float] [arg1 arg2] GL/GL_FLOAT_RGB32_NV )

(defmethod gl_internal_format_from_texture_format_and_type [:alpha :ubyte] [arg1 arg2] GL/GL_ALPHA8 )
(defmethod gl_internal_format_from_texture_format_and_type [:alpha :short] [arg1 arg2] GL/GL_ALPHA16 )
(defmethod gl_internal_format_from_texture_format_and_type [:alpha :half_float] [arg1 arg2] GL/GL_ALPHA16F_ARB )
(defmethod gl_internal_format_from_texture_format_and_type [:alpha :float] [arg1 arg2] GL/GL_ALPHA32F_ARB )

(defmethod gl_internal_format_from_texture_format_and_type [:lum_alpha :ubyte] [arg1 arg2] GL/GL_LUMINANCE_ALPHA8UI_EXT )
(defmethod gl_internal_format_from_texture_format_and_type [:lum_alpha :short] [arg1 arg2] GL/GL_LUMINANCE_ALPHA16UI_EXT  )
(defmethod gl_internal_format_from_texture_format_and_type [:lum_alpha :half_float] [arg1 arg2] GL/GL_LUMINANCE_ALPHA16F_ARB )
(defmethod gl_internal_format_from_texture_format_and_type [:lum_alpha :float] [arg1 arg2] GL/GL_LUMINANCE_ALPHA32F_ARB  )

(defmethod gl_internal_format_from_texture_format_and_type [:lum :ubyte] [arg1 arg2] GL/GL_LUMINANCE8 )
(defmethod gl_internal_format_from_texture_format_and_type [:lum :short] [arg1 arg2] GL/GL_LUMINANCE8UI_EXT )
(defmethod gl_internal_format_from_texture_format_and_type [:lum :half_float] [arg1 arg2] GL/GL_LUMINANCE16F_ARB )
(defmethod gl_internal_format_from_texture_format_and_type [:lum :float] [arg1 arg2] GL/GL_LUMINANCE32F_ARB )

(defn gl_internal_format_from_texture_spec [spec]
  (gl_internal_format_from_texture_format_and_type
   (spec :texture_type)
   (spec :format)))

(defmulti gl_external_format_from_texture_type identity)
(defmethod gl_external_format_from_texture_type :rgba [arg] GL/GL_RGBA )
(defmethod gl_external_format_from_texture_type :rgb [arg] GL/GL_RGB )
(defmethod gl_external_format_from_texture_type :lum [arg] GL/GL_LUMINANCE )
(defmethod gl_external_format_from_texture_type :lum_alpha [arg] GL/GL_LUMINANCE_ALPHA )
(defmethod gl_external_format_from_texture_type :alpha [arg] GL/GL_ALPHA )

(defn gl_external_format_from_texture_spec [spec]
  (gl_external_format_from_texture_type (spec :texture_type)))

(defmulti convert_depth_bits_to_gl_constant identity)
(defmethod convert_depth_bits_to_gl_constant :default [arg] GL/GL_DEPTH_COMPONENT24)
(defmethod convert_depth_bits_to_gl_constant :16      [arg] GL/GL_DEPTH_COMPONENT16)
(defmethod convert_depth_bits_to_gl_constant :32      [arg] GL/GL_DEPTH_COMPONENT32)

;allocate an FBO but don't allocate the texture; resize it if necessary
(defn internal_allocate_opengl_framebuffer_object [gl texture_index tex_handle, surface_spec]
  (let [texture_spec (surface_spec :texture_spec)
	fbo_handle (allocate_opengl_framebuffer_object gl)
	internal_format (gl_internal_format_from_texture_spec texture_spec)
	external_format (gl_external_format_from_texture_spec texture_spec)
	[width height] (texture_spec :size)
	depth_bits (surface_spec :depth_bits)]
    ;game on
    (. gl glBindFramebufferEXT fbo_handle)
    (. gl glBindTexture GL/GL_TEXTURE_2D tex_handle)
    ;allocate space on the card...
    (. gl glTexImage2D GL/GL_TEXTURE_2D 0 internal_format width height 0 external_format nil)
    (. gl glFramebufferTexture2DEXT 
       GL/GL_FRAMEBUFFER_EXT GL/GL_COLOR_ATTACHMENT0_EXT GL/GL_TEXTURE_2D 
       tex_handle 0)
    (when (not (= :none depth_bits))
      (let [depth_constant (convert_depth_bits_to_gl_constant depth_bits)
	    depth_buffer (allocate_opengl_framebuffer_renderbuffer gl)]
	(. gl glBindRenderbufferEXT depth_buffer)
	(. gl glRenderbufferStorageEXT GL/GL_RENDERBUFFER_EXT depth_constant width height)
	(. gl glFramebufferRenderbufferEXT 
	   GL/GL_FRAMEBUFFER_EXT GL/GL_DEPTH_ATTACHMENT_EXT GL/GL_RENDERBUFFER_EXT depth_buffer)))
    ;omitting status check for now until I figure some sequence of formats I can
    ;run through
    (. gl glBindFramebufferEXT 0)
    (create_context_surface surface_spec texture_index fbo_handle)))
  
       
;Takes a gl, a surface spec, and a count of the number of texture currently allocated
;returns a context_surface and a context_texture
(defn allocate_opengl_framebuffer_object [gl surface_spec context_texture_count]
  (let [texture_index context_texture_count
	texture_spec (surface_spec :texture_spec)
	context_texture (allocate_opengl_texture gl texture_spec)
	tex_handle (context_texture :gl_handle)]
    [(internal_allocate_opengl_framebuffer_object texture_index tex_handle surface_spec)
     context_texture]))

;Releases opengl resources related to the context surface
;sets gl handle to an invalid value.
(defn internal_release_context_surface [gl context_surface]
  (create_context_surface 
   (context_surface :surface_spec) 
   (context_surface :texture_index) 
   (release_opengl_framebuffer_object gl (context_surface :gl_handle))))

;release the context surface.
;returns the new list of all surfaces
(defn release_context_surface[gl index all_surfaces]
  (let [context_surface (all_surfaces index)
	new_surface (internal_release_context_surface gl context_surface)]
    (assoc all_surfaces index new_surface)))
 
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
	new_texture_spec (create_texture_spec (context_texture_spec :format)
						  (context_texture_spec :texture_type)
						  newWidth newHeight) ;create new spec with new sizes

	;create a new surface description with the updated texture specifications
	new_surface_spec (create_surface_spec (surface_spec :depth_bits)
					      (surface_spec :stencil)
					      new_texture_spec)

	;create a new context_texture with the new texture description that reflects
	;that we are going to change the size of its data
	new_context_texture (create_context_texture new_texture_spec tex_handle)

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
      (let [[new_surface new_textures_list] (reallocate_opengl_framebuffer_object gl ctx_src surface_spec)
	    new_all_surfaces (assoc all_surfaces index new_surface)]
	[new_all_surfaces new_textures_list])
	[all_surfaces context_textures_list])))

;Allocator must return a context surface if requested
;returns a list of new surface manager along with new context surface index
;re-allocator must take the surface spec, the list of all surfaces, and an index
;that *may* need to be re-allocated if its width or height is less than
;specified in the surface spec. IT should return a new list of all surfaces
;or the current list if no allocation is desired.
(defn allocate_context_surface [gl surface_manager surface_spec context_textures_list]
  (let [all_surfaces (surface_manager :all_surfaces)
	unused_surfaces (surface_manager :unused_surfaces)
	best (find_best_context_surface all_surfaces unused_surfaces)]
    (if (== best -1)
      (let [[retval new_all_surfaces] (find_empty_context_surface all_surfaces)
	    [new_context_surface new_context_texture] (allocate_opengl_framebuffer_object gl surface_spec count)
	    new_all_surfaces (assoc new_all_surfaces retval new_context_surface)
	    new_surface_manager (create_surface_manager new_all_surfaces unused_surfaces)
	    new_context_textures_list (conj context_textures_list new_context_texture)]
	[new_surface_manager new_context_textures_list retval])
      (let [[new_context_surface new_context_textures_list] (reallocate_context_surface_if_necessary 
							     gl best all_surfaces surface_spec
							     context_textures_list)
	    new_unused_surfaces (filter #(== best %) unused_surfaces)
	    new_all_surfaces (assoc all_surfaces best new_context_surface)
	    new_surface_manager (create_surface_manager new_all_surfaces new_unused_surfaces)]
	[new_surface_manager new_context_textures_list best]))))

;Returns the index to the surface manager's unused list.
;Returns a new surface manager
(defn return_surface [surface_manager index]
  (create_surface_manager 
   (surface_manager :all_surfaces) 
   (conj (surface_manager :unused_surfaces) index)))

(defmulti perform_render_command :render_command_type)

(defmethod perform_render_command :default [cmd context]
  (println "unrecognized render command")
  context)
(defmethod perform_render_command :scene_render_command [cmd context]
  (println "rendering the scene")
  context)