(in-ns 'lambinator.rcgl)

;surfaces is a vector of all the known surfaces
;unused is a linked list of the unused surfaces
;render size may be <= surface size
(defstruct context_renderbuffer :gl_handle :texture_gl_handle)
(defstruct context_surface :gl_handle :surface_spec :attachments :framebuffer_status :name )

(defn context_renderbuffer_valid [rb]
  (and rb
       (> (rb :gl_handle) 0)))

(defn context_renderbuffer_texture_valid[rb]
  (and (context_renderbuffer_valid rb)
       (> (rb :texture_gl_handle) 0)))

(defn context_surface_valid[surface]
  (and surface
       (> (surface :gl_handle) 0)))

(defn context_surface_valid_for_render[surface]
  (and (context_surface_valid surface)
       (== (surface :framebuffer_status) GL/GL_FRAMEBUFFER_COMPLETE_EXT)))

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

(defmulti gl_attachment_point_from_rc_attachment_point identity)
(defmethod gl_attachment_point_from_rc_attachment_point :color0 [_] GL/GL_COLOR_ATTACHMENT0_EXT)
(defmethod gl_attachment_point_from_rc_attachment_point :color1 [_] GL/GL_COLOR_ATTACHMENT1_EXT)
(defmethod gl_attachment_point_from_rc_attachment_point :color2 [_] GL/GL_COLOR_ATTACHMENT2_EXT)
(defmethod gl_attachment_point_from_rc_attachment_point :color3 [_] GL/GL_COLOR_ATTACHMENT3_EXT)
(defmethod gl_attachment_point_from_rc_attachment_point :depth [_] GL/GL_DEPTH_ATTACHMENT_EXT)

(defn create_and_bind_renderbuffer [gl internal_format width height attach_pt]
  (let [rb_handle (allocate_opengl_framebuffer_renderbuffer gl)
	gl_attach_pt (gl_attachment_point_from_rc_attachment_point attach_pt)]
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb_handle)
    (. gl glRenderbufferStorageEXT GL/GL_RENDERBUFFER_EXT internal_format width height)
    (. gl glFramebufferRenderbufferEXT 
       GL/GL_FRAMEBUFFER_EXT gl_attach_pt GL/GL_RENDERBUFFER_EXT rb_handle)
    (struct context_renderbuffer rb_handle 0)))

(defn create_and_bind_textured_renderbuffer[gl internal_format width height external_format external_datatype attach_pt binding_func]
  (let [rb_handle (allocate_opengl_framebuffer_renderbuffer gl)
	tex_handle (allocate_opengl_texture_handle gl)]
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb_handle)
    (. gl glBindTexture GL/GL_TEXTURE_2D tex_handle)
    (when binding_func
      (binding_func))
    (. gl glTexImage2D GL/GL_TEXTURE_2D 0 internal_format width height 0 external_format external_datatype nil)
    (. gl glFramebufferTexture2DEXT 
       GL/GL_FRAMEBUFFER_EXT 
       (gl_attachment_point_from_rc_attachment_point attach_pt) GL/GL_TEXTURE_2D 
       tex_handle 0)
    (struct context_renderbuffer rb_handle tex_handle)))	

(defmulti create_context_rb (fn [gl attach_pt renderbuffer width height] [(renderbuffer :type) (renderbuffer :use_texture)]))
(defmethod create_context_rb [:color false] [gl attach_pt renderbuffer width height]
  (let [rc_format (renderbuffer :color_format)
	rc_dtype (renderbuffer :color_datatype)
	internal_format (gl_internal_format_from_texture_format_and_type rc_format rc_dtype)]
    (create_and_bind_renderbuffer gl internal_format width height attach_pt)))

(defmethod create_context_rb [:color true] [gl attach_pt renderbuffer width height]
  (let [rc_format (renderbuffer :color_format)
	rc_dtype (renderbuffer :color_datatype)]
    (let [internal_format (gl_internal_format_from_texture_format_and_type rc_format rc_dtype)
	  external_format (gl_external_format_from_texture_format rc_format)
	  external_datatype (gl_external_datatype_from_texture_datatype rc_dtype)]
      (create_and_bind_textured_renderbuffer gl internal_format width height external_format external_datatype attach_pt nil))))

(defmethod create_context_rb [:depth false] [gl attach_pt renderbuffer width height]
  (let [depth_constant (convert_depth_bits_to_gl_constant (renderbuffer :depth_bits))]
    (create_and_bind_renderbuffer gl depth_constant width height attach_pt)))


(defmethod create_context_rb [:depth true] [gl attach_pt renderbuffer width height]
  (let [rb_handle (allocate_opengl_framebuffer_renderbuffer gl)
	depth_constant (convert_depth_bits_to_gl_constant (renderbuffer :depth_bits))
	internal_format (convert_depth_bits_to_gl_constant (renderbuffer :depth_bits))
	tex_handle (allocate_opengl_texture_handle gl)
	external_format GL/GL_DEPTH_COMPONENT
	external_datatype GL/GL_FLOAT
	binding_func (fn []
		       (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_COMPARE_FUNC GL/GL_LEQUAL) ;write-property ) think
		       (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_DEPTH_TEXTURE_MODE GL/GL_LUMINANCE) ;read-property, not strictly necessary to set here
		       (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_COMPARE_MODE GL/GL_COMPARE_R_TO_TEXTURE))] ;write-property I think
    (create_and_bind_textured_renderbuffer gl internal_format width height external_format external_datatype attach_pt binding_func)))

(defmulti gl_num_samples_from_num_samples identity)
(defmethod gl_num_samples_from_num_samples :default [_] 4)
(defmethod gl_num_samples_from_num_samples :2 [_] 2)
(defmethod gl_num_samples_from_num_samples :8 [_] 8)
(defmethod gl_num_samples_from_num_samples :16 [_] 16)

(defn create_and_bind_multisample_renderbuffer [gl attach_pt width height internal_format num_samples]
  (let [rb_handle (allocate_opengl_framebuffer_renderbuffer gl)
	gl_num_samples (gl_num_samples_from_num_samples num_samples)
	gl_attach_pt (gl_attachment_point_from_rc_attachment_point attach_pt)]
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb_handle)
    (. gl glRenderbufferStorageMultisampleEXT GL/GL_RENDERBUFFER_EXT gl_num_samples internal_format width height)
    (. gl glFramebufferRenderbufferEXT 
       GL/GL_FRAMEBUFFER_EXT gl_attach_pt GL/GL_RENDERBUFFER_EXT rb_handle)
    (struct context_renderbuffer rb_handle 0)))

(defmulti create_multisample_renderbuffer (fn [gl attach_pt renderbuffer width height num_samples] (renderbuffer :type)))

(defmethod create_multisample_renderbuffer :color [gl attach_pt renderbuffer width height num_samples]
  (let [rc_format (renderbuffer :color_format)
	rc_dtype (renderbuffer :color_datatype)
	internal_format (gl_internal_format_from_texture_format_and_type rc_format rc_dtype)]
    (create_and_bind_multisample_renderbuffer gl attach_pt width height internal_format num_samples)))

(defmethod create_multisample_renderbuffer :depth [gl attach_pt renderbuffer width height num_samples]
  (let [internal_format (convert_depth_bits_to_gl_constant (renderbuffer :depth_bits))]
    (create_and_bind_multisample_renderbuffer gl attach_pt width height internal_format num_samples)))

(defn create_context_surface[gl sspec name]
  (let [has_multi_sample (has_multi_sample sspec)
	fbo_handle (allocate_opengl_framebuffer_object gl)
	[width height] (sspec :size)
	num_samples (sspec :num_samples)]
    (try
     (if has_multi_sample
       (println "creating multi-sample context surface")
       (println "creating context surface"))
     (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT fbo_handle)
     (let [context_renderbuffer_mapcat_fn (if has_multi_sample
					    (fn [[attach_pt renderbuffer]]
					      [ attach_pt (create_multisample_renderbuffer gl attach_pt renderbuffer width height num_samples) ])
					    (fn [[attach_pt renderbuffer]]
					      [attach_pt (create_context_rb gl attach_pt renderbuffer width height)]))
	   context_renderbuffers (mapcat context_renderbuffer_mapcat_fn (sspec :attachments))
	   context_renderbuffer_map (if context_renderbuffers
				      (apply assoc {} context_renderbuffers)
				      {})
	   complete (. gl glCheckFramebufferStatusEXT GL/GL_FRAMEBUFFER_EXT)]
       (struct-map context_surface 
	 :gl_handle fbo_handle 
	 :surface_spec sspec 
	 :attachments context_renderbuffer_map 
	 :framebuffer_status complete
	 :name name))
     (catch Exception e 
       ;this is important, if an exception is thrown at a bad time
       ;then unless you release the fbo object your program will
       ;be unable to create more fbo's!
       (release_opengl_framebuffer_object gl fbo_handle) 
       (throw e))
     (finally 
      (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT 0)))))

(defn delete_context_surface[gl ctx_sface]
  (when (context_surface_valid ctx_sface)
    (println "destroying context surface")
    (doseq [[attach_pt context_rb] (ctx_sface :attachments)]
      (when (context_renderbuffer_texture_valid context_rb)
	(release_opengl_texture_handle gl (context_rb :texture_gl_handle)))
      (when (context_renderbuffer_valid context_rb)
	(release_opengl_framebffer_renderbuffer gl (context_rb :gl_handle))))
    (release_opengl_framebuffer_object gl (ctx_sface :gl_handle))
    (struct-map context_surface
      :gl_handle 0
      :attachments {})))

(defn update_context_surface[gl ctx_sface newWidth newHeight]
  (delete_context_surface gl ctx_sface)
  (create_context_surface gl (assoc (ctx_sface :surface_spec) :size [newWidth newHeight]) (ctx_sface :name)))

(defn create_invalid_context_surface [sspec name]
  (struct-map context_surface 
	 :gl_handle 0 
	 :surface_spec sspec 
	 :attachments {} 
	 :framebuffer_status 0
	 :name name))

(defn add_new_context_surface [surfaces_ref sspec name]
  (dosync 
   (let [existing (@surfaces_ref name)]
     (ref-set surfaces_ref (assoc @surfaces_ref name (create_invalid_context_surface sspec name)))
     existing)))
	 
;create a named context surface so you can get at it later.
(defn create_named_context_surface[gl surfaces_ref sspec name]
  (let [existing (add_new_context_surface surfaces_ref sspec name)]
    (when existing
      (delete_context_surface gl existing))
    (let [new_surface (create_context_surface gl sspec name)]
      (dosync (ref-set surfaces_ref (assoc @surfaces_ref name new_surface))))))

;only runs if the surface exists already
(defn update_named_context_surface[gl surfaces_ref name width height]
  (let [existing (@surfaces_ref name)]
    (when existing
      (let [new_surface (update_context_surface gl existing width height)]
	(dosync (ref-set surfaces_ref (assoc @surfaces_ref name new_surface)))))))

(defn remove_and_return_context_surface [surfaces_ref name]
  (dosync
   (let [existing (@surfaces_ref name)]
     (ref-set surfaces_ref (dissoc @surfaces_ref name))
     existing)))

(defn delete_named_context_surface[gl surfaces_ref name]
  (let [existing (remove_and_return_context_surface surfaces_ref name)]
    (delete_context_surface gl existing)))

;This will re-create or create context surfaces such that they
;exactly match the request.
(defn get_or_create_context_surface[gl surfaces_ref sspec name]
  (let [existing (@surfaces_ref name)]
    (if (or (not (context_surface_valid_for_render existing))
	    (not (= sspec (existing :surface_spec))))
      (do
	(delete_named_context_surface gl surfaces_ref name)
	(create_named_context_surface gl surfaces_ref sspec name)
	(@surfaces_ref name))
      existing)))

;bulk re-create the surfaces
(defn context_surfaces_destroyed[gl surfaces_ref]
  (let [new_surfaces (mapcat (fn [[name surface]]
				  [name (create_context_surface gl (surface :surface_spec) (surface :name))])
			     @surfaces_ref)]
    (when new_surfaces
      (ref-set surfaces_ref (apply assoc @surfaces_ref new_surfaces)))))