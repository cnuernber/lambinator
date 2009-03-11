(ns lambinator.rcgl.fbo
  (:use lambinator.rcgl.util lambinator.log lambinator.rcgl
	lambinator.rcgl.texture lambinator.rc
	lambinator.util)
  (:import (javax.media.opengl GL)))

(defn- rcgl-fbo-log [log-data-ref type & args]
  (when log-data-ref
    (log-message @log-data-ref "rcgl.fbo:" type args)))

;A context surfaces is made up of several context renderbuffers
;I use surface and FBO interchangeably
(defstruct context-renderbuffer 
  :gl-handle 
  :texture-gl-handle 
  :gl-error)

(defstruct context-surface 
  :gl-handle 
  :surface-spec 
  :attachments 
  :framebuffer-status 
  :name 
  :gl-error )

(defn rcglf-context-renderbuffer-valid 
  "Return true if the renderbuffer is valid"
  [rb]
  (and rb
       (> (rb :gl-handle) 0)))

(defn rcglf-context-renderbuffer-texture-valid
  "Return true if the renderbuffer is valid and its texture
is valid"
  [rb]
  (and (rcglf-context-renderbuffer-valid rb)
       (> (rb :texture-gl-handle) 0)))

(defn rcglf-context-surface-valid
  "Return true if the context surface is valid"
  [surface]
  (and surface
       (> (surface :gl-handle) 0)))

(defn rcglf-context-surface-valid-for-render
  "Return true if the context surface is valid for render, which
means that it is valid and framebuffer-complete"
  [surface]
  (and (rcglf-context-surface-valid surface)
       (== (surface :framebuffer-status) GL/GL_FRAMEBUFFER_COMPLETE_EXT)))


(defn- allocate-opengl-framebuffer-object [gl]
  (rcglu-allocate-gl-item gl glGenFramebuffersEXT ))

(defn- release-opengl-framebuffer-object [gl fbo-handle]
  (rcglu-release-gl-item gl glDeleteFramebuffersEXT fbo-handle)
  0)

(defn- allocate-opengl-framebuffer-renderbuffer [gl]
  (rcglu-allocate-gl-item gl glGenRenderbuffersEXT ))

(defn- release-opengl-framebuffer-renderbuffer [gl rb-hdl]
  (rcglu-release-gl-item gl glDeleteRenderbuffersEXT rb-hdl))

(defmulti gl-attachment-point-from-rc-attachment-point identity)
(defmethod gl-attachment-point-from-rc-attachment-point :color0 [_] GL/GL_COLOR_ATTACHMENT0_EXT)
(defmethod gl-attachment-point-from-rc-attachment-point :color1 [_] GL/GL_COLOR_ATTACHMENT1_EXT)
(defmethod gl-attachment-point-from-rc-attachment-point :color2 [_] GL/GL_COLOR_ATTACHMENT2_EXT)
(defmethod gl-attachment-point-from-rc-attachment-point :color3 [_] GL/GL_COLOR_ATTACHMENT3_EXT)
(defmethod gl-attachment-point-from-rc-attachment-point :depth [_] GL/GL_DEPTH_ATTACHMENT_EXT)


(defn- create-and-bind-renderbuffer [gl internal-format width height attach-pt]
  (let [rb-handle (allocate-opengl-framebuffer-renderbuffer gl)
	gl-attach-pt (gl-attachment-point-from-rc-attachment-point attach-pt)]
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb-handle)
    (. gl glRenderbufferStorageEXT GL/GL_RENDERBUFFER_EXT internal-format width height)
    (. gl glFramebufferRenderbufferEXT 
       GL/GL_FRAMEBUFFER_EXT gl-attach-pt GL/GL_RENDERBUFFER_EXT rb-handle)
    (struct context-renderbuffer rb-handle 0 (rcglu-get-gl-error gl))))

(defn- create-and-bind-textured-renderbuffer[gl internal-format width height external-format external-datatype attach-pt binding-func]
  (let [rb-handle (allocate-opengl-framebuffer-renderbuffer gl)
	tex-handle (rcglt-allocate-opengl-texture-handle gl)]
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb-handle)
    (. gl glBindTexture GL/GL_TEXTURE_2D tex-handle)
    (when binding-func
      (binding-func))
    (. gl glTexImage2D GL/GL_TEXTURE_2D 0 internal-format width height 0 external-format external-datatype nil)
    (. gl glFramebufferTexture2DEXT 
       GL/GL_FRAMEBUFFER_EXT 
       (gl-attachment-point-from-rc-attachment-point attach-pt) GL/GL_TEXTURE_2D 
       tex-handle 0)
    (struct context-renderbuffer rb-handle tex-handle (rcglu-get-gl-error gl))))


(defmulti create-context-rb (fn [gl attach-pt renderbuffer width height] [(renderbuffer :type) (renderbuffer :use-texture)]))
(defmethod create-context-rb [:color false] [gl attach-pt renderbuffer width height]
  (let [rc-format (renderbuffer :color-format)
	rc-dtype (renderbuffer :color-datatype)
	internal-format (rcglu-gl-internal-format-from-rc-format-and-type rc-format rc-dtype)]
    (create-and-bind-renderbuffer gl internal-format width height attach-pt)))

(defmethod create-context-rb [:color true] [gl attach-pt renderbuffer width height]
  (let [rc-format (renderbuffer :color-format)
	rc-dtype (renderbuffer :color-datatype)]
    (let [internal-format (rcglu-gl-internal-format-from-rc-format-and-type rc-format rc-dtype)
	  external-format (rcglu-gl-format-from-rc-format rc-format)
	  external-datatype (rcglu-gl-datatype-from-rc-datatype rc-dtype)]
      (create-and-bind-textured-renderbuffer gl internal-format width height external-format external-datatype attach-pt nil))))

(defmethod create-context-rb [:depth false] [gl attach-pt renderbuffer width height]
  (let [depth-constant (rcglu-convert-depth-bits-to-gl-constant (renderbuffer :depth-bits))]
    (create-and-bind-renderbuffer gl depth-constant width height attach-pt)))


(defmethod create-context-rb [:depth true] [gl attach-pt renderbuffer width height]
  (let [rb-handle (allocate-opengl-framebuffer-renderbuffer gl)
	internal-format (rcglu-convert-depth-bits-to-gl-constant (renderbuffer :depth-bits))
	tex-handle (rcglt-allocate-opengl-texture-handle gl)
	external-format GL/GL_DEPTH_COMPONENT
	external-datatype GL/GL_FLOAT
	binding-func (fn []
		       (rcglt-tex2d-param gl GL/GL_TEXTURE_COMPARE_FUNC GL/GL_LEQUAL) ;write-property I think
		       (rcglt-tex2d-param gl GL/GL_DEPTH_TEXTURE_MODE GL/GL_LUMINANCE) ;read-property, not strictly necessary to set here
		       (rcglt-tex2d-param gl GL/GL_TEXTURE_COMPARE_MODE GL/GL_COMPARE_R_TO_TEXTURE))] ;write-property I think
    (create-and-bind-textured-renderbuffer gl internal-format width height external-format external-datatype attach-pt binding-func)))


(defmulti gl-num-samples-from-num-samples identity)
(defmethod gl-num-samples-from-num-samples :default [_] 4)
(defmethod gl-num-samples-from-num-samples :2 [_] 2)
(defmethod gl-num-samples-from-num-samples :8 [_] 8) ;nobody supports this yet
(defmethod gl-num-samples-from-num-samples :16 [_] 16) ;nobody supports this

(defn- create-and-bind-multisample-renderbuffer [log-data-ref gl attach-pt width height internal-format num-samples]
  (let [rb-handle (allocate-opengl-framebuffer-renderbuffer gl)
	gl-num-samples (gl-num-samples-from-num-samples num-samples)
	gl-attach-pt (gl-attachment-point-from-rc-attachment-point attach-pt)]
    (rcgl-fbo-log log-data-ref :info "allocating multisample renderbuffer (samples): " num-samples )
    (. gl glBindRenderbufferEXT GL/GL_RENDERBUFFER_EXT rb-handle)
    (. gl glRenderbufferStorageMultisampleEXT GL/GL_RENDERBUFFER_EXT gl-num-samples internal-format width height)
    (. gl glFramebufferRenderbufferEXT 
       GL/GL_FRAMEBUFFER_EXT gl-attach-pt GL/GL_RENDERBUFFER_EXT rb-handle)
    (struct context-renderbuffer rb-handle 0 (rcglu-get-gl-error gl))))

(defn- create-multisample-renderbuffer-dispatch [log-data-ref gl attach-pt renderbuffer & args] (renderbuffer :type))

(defmulti create-multisample-renderbuffer create-multisample-renderbuffer-dispatch )

(defmethod create-multisample-renderbuffer :color [log-data-ref gl attach-pt renderbuffer width height num-samples]
  (let [rc-format (renderbuffer :color-format)
	rc-dtype (renderbuffer :color-datatype)
	internal-format (rcglu-gl-internal-format-from-rc-format-and-type rc-format rc-dtype)]
    (create-and-bind-multisample-renderbuffer log-data-ref gl attach-pt width height internal-format num-samples)))

(defmethod create-multisample-renderbuffer :depth [log-data-ref gl attach-pt renderbuffer width height num-samples]
  (let [internal-format (rcglu-convert-depth-bits-to-gl-constant (renderbuffer :depth-bits))]
    (create-and-bind-multisample-renderbuffer log-data-ref gl attach-pt width height internal-format num-samples)))

(defn rcglf-create-context-surface
  "Create a context surface from an rc.surface-spec.  This function may fail; it
is relatively easy to create invalid surface specs"
  [log-data-ref gl sspec name]
  (let [has-multi-sample (has-multi-sample sspec)
	fbo-handle (allocate-opengl-framebuffer-object gl)
	[width height] (sspec :size)
	num-samples (sspec :multi-sample)]
    (try
     (if has-multi-sample
       (rcgl-fbo-log log-data-ref :info "creating multi-sample context surface: " name " , number of samples: " num-samples )
       (rcgl-fbo-log log-data-ref :info "creating context surface: " name))
     (. gl glEnable GL/GL_TEXTURE_2D)
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
       (rcgl-fbo-log log-data-ref :info "Framebuffer complete: " (rcglu-get-opengl-constant-name complete))
       (struct-map context-surface 
	 :gl-handle fbo-handle 
	 :surface-spec sspec 
	 :attachments context-renderbuffer-map 
	 :framebuffer-status complete
	 :name name
	 :gl-error (rcglu-get-gl-error gl)))
     (catch Exception e 
       ;this is important, if an exception is thrown at a bad time
       ;then unless you release the fbo object your program will
       ;be unable to create more fbo's!
       (release-opengl-framebuffer-object log-data-ref gl fbo-handle) 
       (throw e))
     (finally 
      (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT 0)))))

(defn rcglf-delete-context-surface
  "Delete a given context surface.  Returns a new context surface"
  [log-data-ref gl ctx-sface]
  (when (rcglf-context-surface-valid ctx-sface)
    (rcgl-fbo-log log-data-ref :info "deleting context surface: " (ctx-sface :name))
    (doseq [[attach-pt context-rb] (ctx-sface :attachments)]
      (when (rcglf-context-renderbuffer-texture-valid context-rb)
	(rcglt-release-opengl-texture-handle gl (context-rb :texture-gl-handle)))
      (when (rcglf-context-renderbuffer-valid context-rb)
	(release-opengl-framebuffer-renderbuffer gl (context-rb :gl-handle))))
    (release-opengl-framebuffer-object gl (ctx-sface :gl-handle))
    ;clear errors in case this was an invalid handle
    (struct-map context-surface
      :gl-handle 0
      :attachments {}
      :gl-error (rcglu-get-gl-error gl))))

(defn rcglf-update-context-surface
  "Update a given context surface by changing its dimensions"
  [log-data-ref gl ctx-sface newWidth newHeight]
  (rcglf-delete-context-surface log-data-ref gl ctx-sface)
  (rcglf-create-context-surface log-data-ref gl (assoc (ctx-sface :surface-spec) :size [newWidth newHeight]) (ctx-sface :name)))


(defn- create-invalid-context-surface [sspec name]
  (struct-map context-surface 
	 :gl-handle 0 
	 :surface-spec sspec 
	 :attachments {} 
	 :framebuffer-status 0
	 :name name))

(defn- add-new-context-surface [surfaces-ref sspec name]
  (dosync 
   (let [existing (@surfaces-ref name)]
     (ref-set surfaces-ref (assoc @surfaces-ref name (create-invalid-context-surface sspec name)))
     existing)))
	 
;create a named context surface so you can get at it later.
(defn rcglf-create-named-context-surface
  "Create a named context surface, updates the surfaces map with the result"
  [log-data-ref gl surfaces-ref sspec name]
  (let [existing (add-new-context-surface surfaces-ref sspec name)]
    (when existing
      (rcglf-delete-context-surface log-data-ref gl existing))
    (let [new-surface (rcglf-create-context-surface log-data-ref gl sspec name)]
      (dosync (ref-set surfaces-ref (assoc @surfaces-ref name new-surface))))))


;only runs if the surface exists already
(defn rcglf-update-named-context-surface
  "Update a named context surface.  You can resize a surface; this will result in
a deletion and new allocation"
  [log-data-ref gl surfaces-ref name width height]
  (let [existing (@surfaces-ref name)]
    (when existing
      (let [new-surface (rcglf-update-context-surface log-data-ref gl existing width height)]
	(dosync (ref-set surfaces-ref (assoc @surfaces-ref name new-surface)))))))

(defn- remove-and-return-context-surface [surfaces-ref name]
  (dosync
   (let [existing (@surfaces-ref name)]
     (ref-set surfaces-ref (dissoc @surfaces-ref name))
     existing)))

(defn rcglf-delete-named-context-surface
  "Delete a named context surface if it exists"
  [log-data-ref gl surfaces-ref name]
  (let [existing (remove-and-return-context-surface surfaces-ref name)]
    (rcglf-delete-context-surface log-data-ref gl existing)))

(defn rcglf-create-named-context-surface-seq
  "Create a named context surface from the sequence of surface specs.  The
first surface specification that results in a valid context surface will be used.
Opengl errors will be cleared after this function"
  [log-data-ref gl surfaces-ref sspec-seq name]
  (first (filter (fn [sspec]
		   (rcglf-create-named-context-surface log-data-ref gl surfaces-ref sspec name) ;create the object.  side effect alert.
		   (= ((@surfaces-ref name) :framebuffer-status) GL/GL_FRAMEBUFFER_COMPLETE_EXT)) ;if it is complete, then stop
		 sspec-seq))
  (when (not (= ((@surfaces-ref name) :framebuffer-status) GL/GL_FRAMEBUFFER_COMPLETE_EXT))
    (rcglf-delete-named-context-surface log-data-ref gl surfaces-ref name))
  nil) ;none of the surfaces were complete.  this avoides gl errors...

;bulk re-create the surfaces
(defn rcglf-context-surfaces-destroyed[log-data-ref gl surfaces-ref]
  (let [new-surfaces (mapcat (fn [[name surface]]
			       [name (rcglf-create-context-surface log-data-ref gl (surface :surface-spec) (surface :name))])
			     @surfaces-ref)]
    (util-update-map-ref surfaces-ref new-surfaces)))