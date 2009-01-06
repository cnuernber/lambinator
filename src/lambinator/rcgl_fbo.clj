(in-ns 'lambinator.rcgl)

(defn rcgl-fbo-log [log-data-ref type & args]
  (when log-data-ref
    (log-message @log-data-ref "rcgl.fbo:" type args)))

;surfaces is a vector of all the known surfaces
;unused is a linked list of the unused surfaces
;render size may be <= surface size
(defstruct context-renderbuffer :gl-handle :texture-gl-handle :gl-error)
(defstruct context-surface :gl-handle :surface-spec :attachments :framebuffer-status :name :gl-error )

(defn context-renderbuffer-valid [rb]
  (and rb
       (> (rb :gl-handle) 0)))

(defn context-renderbuffer-texture-valid[rb]
  (and (context-renderbuffer-valid rb)
       (> (rb :texture-gl-handle) 0)))

(defn context-surface-valid[surface]
  (and surface
       (> (surface :gl-handle) 0)))

(defn context-surface-valid-for-render[surface]
  (and (context-surface-valid surface)
       (== (surface :framebuffer-status) GL/GL_FRAMEBUFFER_COMPLETE_EXT)))

;takes a gl and returns a framebuffer index
(defn allocate-opengl-framebuffer-object [gl]
  (allocate-gl-item (fn [count args offset] (. gl glGenFramebuffersEXT count args offset))))

;takes a gl and a fbo handle, calls release and returns an invalid handle value
(defn release-opengl-framebuffer-object [gl fbo-handle]
  (release-gl-item (fn [count args offset] (. gl glDeleteFramebuffersEXT count args offset)) fbo-handle)
  0)

(defn allocate-opengl-framebuffer-renderbuffer [gl]
  (allocate-gl-item (fn [count args offset] (. gl glGenRenderbuffersEXT count args offset))))

(defn release-opengl-framebffer-renderbuffer [gl rb-hdl]
  (release-gl-item (fn [count args offset] (. gl glDeleteRenderbuffersEXT count args offset)) rb-hdl))

(defmulti gl-attachment-point-from-rc-attachment-point identity)
(defmethod gl-attachment-point-from-rc-attachment-point :color0 [-] GL/GL_COLOR_ATTACHMENT0_EXT)
(defmethod gl-attachment-point-from-rc-attachment-point :color1 [-] GL/GL_COLOR_ATTACHMENT1_EXT)
(defmethod gl-attachment-point-from-rc-attachment-point :color2 [-] GL/GL_COLOR_ATTACHMENT2_EXT)
(defmethod gl-attachment-point-from-rc-attachment-point :color3 [-] GL/GL_COLOR_ATTACHMENT3_EXT)
(defmethod gl-attachment-point-from-rc-attachment-point :depth [-] GL/GL_DEPTH_ATTACHMENT_EXT)

(defn create-and-bind-renderbuffer [gl internal-format width height attach-pt]
  (let [rb-handle (allocate-opengl-framebuffer-renderbuffer gl)
	gl-attach-pt (gl-attachment-point-from-rc-attachment-point attach-pt)]
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb-handle)
    (. gl glRenderbufferStorageEXT GL/GL_RENDERBUFFER_EXT internal-format width height)
    (. gl glFramebufferRenderbufferEXT 
       GL/GL_FRAMEBUFFER_EXT gl-attach-pt GL/GL_RENDERBUFFER_EXT rb-handle)
    (struct context-renderbuffer rb-handle 0 (get-gl-error gl))))

(defn create-and-bind-textured-renderbuffer[gl internal-format width height external-format external-datatype attach-pt binding-func]
  (let [rb-handle (allocate-opengl-framebuffer-renderbuffer gl)
	tex-handle (allocate-opengl-texture-handle gl)]
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb-handle)
    (. gl glBindTexture GL/GL_TEXTURE_2D tex-handle)
    (when binding-func
      (binding-func))
    (. gl glTexImage2D GL/GL_TEXTURE_2D 0 internal-format width height 0 external-format external-datatype nil)
    (. gl glFramebufferTexture2DEXT 
       GL/GL_FRAMEBUFFER_EXT 
       (gl-attachment-point-from-rc-attachment-point attach-pt) GL/GL_TEXTURE_2D 
       tex-handle 0)
    (struct context-renderbuffer rb-handle tex-handle (get-gl-error gl))))	

(defmulti create-context-rb (fn [gl attach-pt renderbuffer width height] [(renderbuffer :type) (renderbuffer :use-texture)]))
(defmethod create-context-rb [:color false] [gl attach-pt renderbuffer width height]
  (let [rc-format (renderbuffer :color-format)
	rc-dtype (renderbuffer :color-datatype)
	internal-format (gl-internal-format-from-texture-format-and-type rc-format rc-dtype)]
    (create-and-bind-renderbuffer gl internal-format width height attach-pt)))

(defmethod create-context-rb [:color true] [gl attach-pt renderbuffer width height]
  (let [rc-format (renderbuffer :color-format)
	rc-dtype (renderbuffer :color-datatype)]
    (let [internal-format (gl-internal-format-from-texture-format-and-type rc-format rc-dtype)
	  external-format (gl-external-format-from-texture-format rc-format)
	  external-datatype (gl-external-datatype-from-texture-datatype rc-dtype)]
      (create-and-bind-textured-renderbuffer gl internal-format width height external-format external-datatype attach-pt nil))))

(defmethod create-context-rb [:depth false] [gl attach-pt renderbuffer width height]
  (let [depth-constant (convert-depth-bits-to-gl-constant (renderbuffer :depth-bits))]
    (create-and-bind-renderbuffer gl depth-constant width height attach-pt)))


(defmethod create-context-rb [:depth true] [gl attach-pt renderbuffer width height]
  (let [rb-handle (allocate-opengl-framebuffer-renderbuffer gl)
	depth-constant (convert-depth-bits-to-gl-constant (renderbuffer :depth-bits))
	internal-format (convert-depth-bits-to-gl-constant (renderbuffer :depth-bits))
	tex-handle (allocate-opengl-texture-handle gl)
	external-format GL/GL_DEPTH_COMPONENT
	external-datatype GL/GL_FLOAT
	binding-func (fn []
		       (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_COMPARE_FUNC GL/GL_LEQUAL) ;write-property ) think
		       (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_DEPTH_TEXTURE_MODE GL/GL_LUMINANCE) ;read-property, not strictly necessary to set here
		       (. gl glTexParameteri GL/GL_TEXTURE_2D GL/GL_TEXTURE_COMPARE_MODE GL/GL_COMPARE_R_TO_TEXTURE))] ;write-property I think
    (create-and-bind-textured-renderbuffer gl internal-format width height external-format external-datatype attach-pt binding-func)))

(defmulti gl-num-samples-from-num-samples identity)
(defmethod gl-num-samples-from-num-samples :default [-] 4)
(defmethod gl-num-samples-from-num-samples :2 [-] 2)
(defmethod gl-num-samples-from-num-samples :8 [-] 8)
(defmethod gl-num-samples-from-num-samples :16 [-] 16)

(defn create-and-bind-multisample-renderbuffer [log-data-ref gl attach-pt width height internal-format num-samples]
  (let [rb-handle (allocate-opengl-framebuffer-renderbuffer gl)
	gl-num-samples (gl-num-samples-from-num-samples num-samples)
	gl-attach-pt (gl-attachment-point-from-rc-attachment-point attach-pt)]
    (rcgl-fbo-log log-data-ref :info "allocating multisample renderbuffer (samples): " num-samples )
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb-handle)
    (. gl glRenderbufferStorageMultisampleEXT GL/GL_RENDERBUFFER_EXT gl-num-samples internal-format width height)
    (. gl glFramebufferRenderbufferEXT 
       GL/GL_FRAMEBUFFER_EXT gl-attach-pt GL/GL_RENDERBUFFER_EXT rb-handle)
    (struct context-renderbuffer rb-handle 0 (get-gl-error gl))))

(defn create-multisample-renderbuffer-dispatch [log-data-ref gl attach-pt renderbuffer & args] (renderbuffer :type))

(defmulti create-multisample-renderbuffer create-multisample-renderbuffer-dispatch )

(defmethod create-multisample-renderbuffer :color [log-data-ref gl attach-pt renderbuffer width height num-samples]
  (let [rc-format (renderbuffer :color-format)
	rc-dtype (renderbuffer :color-datatype)
	internal-format (gl-internal-format-from-texture-format-and-type rc-format rc-dtype)]
    (create-and-bind-multisample-renderbuffer log-data-ref gl attach-pt width height internal-format num-samples)))

(defmethod create-multisample-renderbuffer :depth [log-data-ref gl attach-pt renderbuffer width height num-samples]
  (let [internal-format (convert-depth-bits-to-gl-constant (renderbuffer :depth-bits))]
    (create-and-bind-multisample-renderbuffer log-data-ref gl attach-pt width height internal-format num-samples)))

(defn create-context-surface[log-data-ref gl sspec name]
  (let [has-multi-sample (has-multi-sample sspec)
	fbo-handle (allocate-opengl-framebuffer-object gl)
	[width height] (sspec :size)
	num-samples (sspec :multi-sample)]
    (try
     (if has-multi-sample
       (rcgl-fbo-log log-data-ref :info "creating multi-sample context surface: " name " , number of samples: " num-samples )
       (rcgl-fbo-log log-data-ref :info "creating context surface: " name))
     (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT fbo-handle)
     (let [context-renderbuffer-mapcat-fn (if has-multi-sample
					    (fn [[attach-pt renderbuffer]]
					      [ attach-pt (create-multisample-renderbuffer log-data-ref gl attach-pt renderbuffer width height num-samples) ])
					    (fn [[attach-pt renderbuffer]]
					      [attach-pt (create-context-rb gl attach-pt renderbuffer width height)]))
	   context-renderbuffers (mapcat context-renderbuffer-mapcat-fn (sspec :attachments))
	   context-renderbuffer-map (if context-renderbuffers
				      (apply assoc {} context-renderbuffers)
				      {})
	   complete (. gl glCheckFramebufferStatusEXT GL/GL_FRAMEBUFFER_EXT)]
       (rcgl-fbo-log log-data-ref :info "Framebuffer complete: " (get-opengl-constant-name complete))
       (struct-map context-surface 
	 :gl-handle fbo-handle 
	 :surface-spec sspec 
	 :attachments context-renderbuffer-map 
	 :framebuffer-status complete
	 :name name
	 :gl-error (get-gl-error gl)))
     (catch Exception e 
       ;this is important, if an exception is thrown at a bad time
       ;then unless you release the fbo object your program will
       ;be unable to create more fbo's!
       (release-opengl-framebuffer-object log-data-ref gl fbo-handle) 
       (throw e))
     (finally 
      (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT 0)))))

(defn delete-context-surface[log-data-ref gl ctx-sface]
  (when (context-surface-valid ctx-sface)
    (rcgl-fbo-log log-data-ref :info "deleting context surface: " (ctx-sface :name))
    (doseq [[attach-pt context-rb] (ctx-sface :attachments)]
      (when (context-renderbuffer-texture-valid context-rb)
	(release-opengl-texture-handle gl (context-rb :texture-gl-handle)))
      (when (context-renderbuffer-valid context-rb)
	(release-opengl-framebffer-renderbuffer gl (context-rb :gl-handle))))
    (release-opengl-framebuffer-object gl (ctx-sface :gl-handle))
    ;clear errors in case this was an invalid handle
    (struct-map context-surface
      :gl-handle 0
      :attachments {}
      :gl-error (get-gl-error gl))))

(defn update-context-surface[log-data-ref gl ctx-sface newWidth newHeight]
  (delete-context-surface log-data-ref gl ctx-sface)
  (create-context-surface log-data-ref gl (assoc (ctx-sface :surface-spec) :size [newWidth newHeight]) (ctx-sface :name)))

(defn create-invalid-context-surface [sspec name]
  (struct-map context-surface 
	 :gl-handle 0 
	 :surface-spec sspec 
	 :attachments {} 
	 :framebuffer-status 0
	 :name name))

(defn add-new-context-surface [surfaces-ref sspec name]
  (dosync 
   (let [existing (@surfaces-ref name)]
     (ref-set surfaces-ref (assoc @surfaces-ref name (create-invalid-context-surface sspec name)))
     existing)))
	 
;create a named context surface so you can get at it later.
(defn create-named-context-surface[log-data-ref gl surfaces-ref sspec name]
  (let [existing (add-new-context-surface surfaces-ref sspec name)]
    (when existing
      (delete-context-surface log-data-ref gl existing))
    (let [new-surface (create-context-surface log-data-ref gl sspec name)]
      (dosync (ref-set surfaces-ref (assoc @surfaces-ref name new-surface))))))


;only runs if the surface exists already
(defn update-named-context-surface[log-data-ref gl surfaces-ref name width height]
  (let [existing (@surfaces-ref name)]
    (when existing
      (let [new-surface (update-context-surface log-data-ref gl existing width height)]
	(dosync (ref-set surfaces-ref (assoc @surfaces-ref name new-surface)))))))

(defn remove-and-return-context-surface [surfaces-ref name]
  (dosync
   (let [existing (@surfaces-ref name)]
     (ref-set surfaces-ref (dissoc @surfaces-ref name))
     existing)))

(defn delete-named-context-surface[log-data-ref gl surfaces-ref name]
  (let [existing (remove-and-return-context-surface surfaces-ref name)]
    (delete-context-surface log-data-ref gl existing)))

(defn create-named-context-surface-seq[log-data-ref gl surfaces-ref sspec-seq name]
  (first (filter (fn [sspec]
		   (create-named-context-surface log-data-ref gl surfaces-ref sspec name) ;create the object.  side effect alert.
		   (= ((@surfaces-ref name) :framebuffer-status) GL/GL_FRAMEBUFFER_COMPLETE_EXT)) ;if it is complete, then stop
		 sspec-seq))
  (when (not (= ((@surfaces-ref name) :framebuffer-status) GL/GL_FRAMEBUFFER_COMPLETE_EXT))
    (delete-named-context-surface log-data-ref gl surfaces-ref name))
  nil) ;none of the surfaces were complete.  this avoides gl errors...

;This will re-create or create context surfaces such that they
;exactly match the request.
(defn get-or-create-context-surface[log-data-ref gl surfaces-ref sspec name]
  (let [existing (@surfaces-ref name)]
    (if (or (not (context-surface-valid-for-render existing))
	    (not (= sspec (existing :surface-spec))))
      (do
	(delete-named-context-surface log-data-ref gl surfaces-ref name)
	(create-named-context-surface log-data-ref gl surfaces-ref sspec name)
	(@surfaces-ref name))
      existing)))

;bulk re-create the surfaces
(defn context-surfaces-destroyed[log-data-ref gl surfaces-ref]
  (let [new-surfaces (mapcat (fn [[name surface]]
				  [name (create-context-surface log-data-ref gl (surface :surface-spec) (surface :name))])
			     @surfaces-ref)]
    (when new-surfaces
      (dosync (ref-set surfaces-ref (apply assoc @surfaces-ref new-surfaces))))))