(ns lambinator.demo.util
  (:import (javax.media.opengl GL DebugGL)
	   (javax.media.opengl.glu GLU))
  (:use lambinator.rcgl
	lambinator.rc
	lambinator.rcgl.fbo
	lambinator.rcgl.vbo
	lambinator.rcgl.glsl
	lambinator.rcgl.texture
	lambinator.rcgl.util
	lambinator.ui
	lambinator.ui.gl
	lambinator.ui.inspector
	lambinator.util))

(defstruct multisample-data ;anti-alias data
  :frame ;The frame
  :num-samples-ref ;num of samples to render
  :inspector-item ;inspector items.  It is up to the client to add these
)

(defn generate-multisample-vbo[]
  (map 
   float
   [-1 -1  0 0
    -1  1  0 1
     1  1  1 1
     1 -1  1 0]))

(defonce aa-choices-array [:none :2 :4 :8 :16])

(defn- create-multisample-fbos[multisample-data]
  (let [{ { { drawable :gl-win :as win-data } :win-data } :frame 
	  num-samples-ref :num-samples-ref } multisample-data
	width (. drawable getWidth)
	height (. drawable getHeight)
	num-samples @num-samples-ref
	indexed-choices-array (map vector (iterate inc 0) (rest aa-choices-array))
	[this-choice-index _] (first (filter (fn [[index choice]] (= choice num-samples)) indexed-choices-array))
	;pick aa modes that are less than or equal to the chosen aa mode so we have valid fallbacks
	valid-choices (reverse (filter (fn [[index choice]] (>= this-choice-index index)) indexed-choices-array))
	ms-spec-seq (map (fn [[index choice]]
			   (create-surface-spec 
			    { :color0 (create-renderbuffer :color :ubyte :rgb false) }
			    width
			    height
			    choice))
			 valid-choices )
	trans-spec-seq [ (create-surface-spec
			  { :color0 (create-renderbuffer :color :ubyte :rgb  true) }
			  width
			  height)
			 (create-surface-spec
			  { :color0 (create-renderbuffer :color :ubyte :rgb  false) }
			  width
			  height)]]
    (uigl-with-render-context-ref-and-todo-list-ref  
     win-data
     (fn [rc rl]
       (when (first ms-spec-seq)
	 (rcgl-create-context-surface-seq rc rl ms-spec-seq "wave-multisample-surface"))
       (rcgl-create-context-surface-seq rc rl trans-spec-seq "wave-transfer-surface")))))

(defn dmut-create-multisample-render-data
  "Create the data necessary for a multisample render pass"
  [multisample-data]
  (create-multisample-fbos multisample-data)
  (uigl-with-render-context-ref-and-todo-list-ref 
   ((multisample-data :frame) :win-data)
   (fn [rc rl]
     (rcgl-create-glsl-program 
      rc 
      rl
      "/data/glsl/passthrough.glslv" 
      "/data/glsl/single_texture.glslf"
      "wave-final-render-prog")
     (rcgl-create-vbo rc rl "wave-multisample-vbo" :data #(generate-multisample-vbo))))
  nil)

(defn dmut-delete-multisample-render-data
  "Destroy the data necessary for a multisample render pass"
  [multisample-data]
  (uigl-with-render-context-ref-and-todo-list-ref 
   ((multisample-data :frame) :win-data)
   (fn [rc rl]
     (rcgl-delete-context-surface rc rl "wave-multisample-surface")
     (rcgl-delete-context-surface rc rl "wave-transfer-surface")
     (rcgl-delete-glsl-program rc rl "wave-final-render-prog")
     (rcgl-delete-vbo rc rl "wave-multisample-vbo")))
  nil)

(defn- get-or-create-transfer-texture[gl render-context-ref size]
  (let [texture-map-ref (@render-context-ref :texture-map-ref)
	ms-texture (@texture-map-ref "multisample-texture")
	create-new (or (nil? ms-texture)
		       (not
			(and
			  (== (size 0)
			     (((ms-texture :texture-spec) :size) 0))
			  (== (size 1)
			      (((ms-texture :texture-spec) :size) 1)))))]
    (if create-new
      (do
	(rcglt-create-named-texture 
	 gl 
	 texture-map-ref 
	 (struct-map texture-spec 
	   :datatype :ubyte 
	   :format :rgb 
	   :size size)
	 "multisample-texture")
	((@texture-map-ref "multisample-texture") :gl-handle))
      (ms-texture :gl-handle))))
			     
	

;;On windows I am currently unable to allocate an FBO that has a texture
;attachment.  I can't fathom why this is; most likely I have some gl-state
;that isn't being initialized correctly
(defn- maybe-transfer-texture[#^GL gl transfer-surface render-context-ref]
  (. gl glActiveTexture GL/GL_TEXTURE0)
  (let [transfer-tex (((transfer-surface :attachments) :color0) :texture-gl-handle)]
    (if (> transfer-tex 0)
      (do
	(. gl glBindTexture GL/GL_TEXTURE_2D transfer-tex)
	transfer-tex)
      (let [size ((transfer-surface :surface-spec) :size)
	    ms-texture (get-or-create-transfer-texture 
			gl 
			render-context-ref 
			size )
	    rc-dtype :ubyte
	    rc-format :rbg
	    internal-format (rcglu-gl-internal-format-from-rc-format-and-type rc-format rc-dtype)
	    [width height] size]
	(. gl glBindTexture GL/GL_TEXTURE_2D ms-texture)
	;(. gl glReadBuffer GL/GL_COLOR_ATTACHMENT0_EXT)
	(. gl glCopyTexImage2D GL/GL_TEXTURE_2D 0 internal-format 0 0 width height 0)))))
      
(defn- antialiasing-drawable-wrapper
  [drawable render-context-ref frame-resize-data multisample-data child-drawable]
  (let [real-gl (. drawable getGL)
	#^GL gl (DebugGL. real-gl)
	width (. drawable getWidth)
	height (. drawable getHeight)
	render-context @render-context-ref
	ms-surface (rcgl-get-context-surface render-context "wave-multisample-surface")
	transfer-surface (rcgl-get-context-surface render-context "wave-transfer-surface")
	final-prog (rcgl-get-glsl-program render-context "wave-final-render-prog")
	ms-vbo (rcgl-get-vbo render-context "wave-multisample-vbo")
	do-aa-render (and ms-surface transfer-surface final-prog child-drawable ms-vbo)]
    (if do-aa-render
      (let [ms-fbo (ms-surface :gl-handle)
	    transfer-fbo (transfer-surface :gl-handle)
	    prog-handle (final-prog :gl-handle)
	    tex-att ((final-prog :attributes) "input_tex_coords")
	    vertex-att ((final-prog :attributes) "input_vertex_coords")
	    [render-width render-height] ((ms-surface :surface-spec) :size)
	    vbo-dtype (int (ms-vbo :gl-datatype))
	    int-array (make-array Integer/TYPE 5)]
	;have the child render to the multisample fbo
	;the bind function sets where gl will render to
	(. gl glDisable GL/GL_SCISSOR_TEST)
	(. gl glGetIntegerv GL/GL_FRAMEBUFFER_BINDING_EXT int-array 0)
	(. gl glGetIntegerv GL/GL_VIEWPORT int-array 1)
	(try
	 (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT ms-fbo)
	 (. gl glViewport 0 0 render-width render-height)
	 (child-drawable drawable)
					;bind the multisample framebuffer as the read framebuffer source
	 (. gl glBindFramebufferEXT GL/GL_READ_FRAMEBUFFER_EXT ms-fbo)
					;bind the transfer as the draw framebuffer dest
	 (. gl glBindFramebufferEXT GL/GL_DRAW_FRAMEBUFFER_EXT transfer-fbo);
        ;downsample the multisample fbo to the draw framebuffer's texture
	 (. gl glBlitFramebufferEXT 
	    0 0 render-width render-height ;source rect
	    0 0 render-width render-height ;dest rect
	    GL/GL_COLOR_BUFFER_BIT ;what to copy over (just color in our case)
	    GL/GL_NEAREST ) ;how to interpolate intermediate results (there aren't any; the sizes match)
	 (. gl glEnable GL/GL_TEXTURE_2D)
	 (. gl glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT transfer-fbo);
	 (maybe-transfer-texture gl transfer-surface render-context-ref)
	 (. gl glEnable GL/GL_SCISSOR_TEST)
	 ;Ensure we don't use the buffer as an accumulation buffer
	 (. gl glDisable GL/GL_BLEND)
         ;Bind the window's render surface as the target render surface
	 (. gl GL/glBindFramebufferEXT GL/GL_FRAMEBUFFER_EXT (aget int-array 0))
	 (. gl glViewport (aget int-array 1) (aget int-array 2) (aget int-array 3) (aget int-array 4))
	 ;Now we render our fullscreen quad
	 (. gl glShadeModel GL/GL_SMOOTH)
	 (. gl glPolygonMode GL/GL_FRONT_AND_BACK GL/GL_FILL)
	 (. gl glMatrixMode GL/GL_PROJECTION)
	 (. gl glLoadIdentity )
	 (. gl glMatrixMode GL/GL_MODELVIEW)
	 (. gl glLoadIdentity)
	 (. gl glUseProgram prog-handle)

	 (. gl glBindBuffer (rcglv-gl-type-from-vbo-type (ms-vbo :type)) (ms-vbo :gl-handle))
	 
	 (rcglt-tex2d-param gl GL/GL_TEXTURE_MIN_FILTER GL/GL_LINEAR)
	 (rcglt-tex2d-param gl GL/GL_TEXTURE_MAG_FILTER GL/GL_LINEAR)
	 (rcglt-tex2d-param gl GL/GL_TEXTURE_WRAP_S GL/GL_CLAMP_TO_EDGE)
	 (rcglt-tex2d-param gl GL/GL_TEXTURE_WRAP_T GL/GL_CLAMP_TO_EDGE)
         ;we have how bound the second set of texture coordinates to tex coord 0
	 ;each tex coord takes two entries, they have a stride of 4
	 ;and they are offset from the beginning of the array by two
	 (when vertex-att
	   (. gl glEnableVertexAttribArray (vertex-att :index))
	   (. gl glVertexAttribPointer 
	      (vertex-att :index) ;index
	      (int 2)       ;size
	      vbo-dtype     ;type
	      false         ;normalized
	      (int 16)       ;stride
	      (long 0)))      ;offset
	 (when tex-att
	   (. gl glEnableVertexAttribArray (tex-att :index))
	   (. gl glVertexAttribPointer 
	      (tex-att :index) ;index
	      (int 2)       ;size
	      vbo-dtype     ;type
	      false         ;normalized
	      (int 16)       ;stride
	      (long 8)))      ;offset
	 (rcgl-set-glsl-uniforms
	  @render-context-ref
	  gl
	  [["tex" 0]] ;set the texture param to desired logical texture unit
	  final-prog )
	  ; Render Fullscreen Quad
	 
	 ;(. gl glClearColor 1.0 1.0 0.0 1.0)
	 ;(. gl glClear GL/GL_COLOR_BUFFER_BIT)
	 ;glDrawArrays takes the index count, not the polygon count or the array item count
	 (. gl glDrawArrays GL/GL_QUADS 0 (/ (ms-vbo :item-count) 4)) ;each index has an x and y, u and v
	 (. gl glBindTexture GL/GL_TEXTURE_2D 0)
	 (catch Exception e 
	   (.printStackTrace e)))) ;make goddamn sure we don't end up with an invalid fbo bound.
      (when child-drawable ;if we can't render antialiased because we don't have buffers
	(child-drawable drawable)))
    ;update frame resize data so we know how many times the drawable has rendered at this exact size
    ;we only resize when a certain number of frames have been rendered at a certain size.
    ;this is because resizing fbos is relative expensive and can apparently lead
    ;to fragmentation of video ram (although I doubt the second claim)
    (let [resize-frame-count (@frame-resize-data :resize-frame-count)
	  [rs-width rs-height] (@frame-resize-data :resize-frame-size)
	  resize-frame-count (if (and (== rs-width width)
				      (== rs-height height))
			       (inc resize-frame-count)
			       0)
	  fbos-missing (or (not ms-surface)
			   (not transfer-surface))
	  fbos-size-mismatch (or fbos-missing
				 (not (= ((ms-surface :surface-spec) :size)
					 [width height])))
	  do-resize-fbo (or fbos-missing
			    (and fbos-size-mismatch
				 (> resize-frame-count 10)))]
      (dosync (ref-set frame-resize-data (assoc @frame-resize-data 
					   :resize-frame-count resize-frame-count
					   :resize-frame-size [width height])))
      (when do-resize-fbo
	(create-multisample-fbos multisample-data)))
    (when ms-surface
      (let [actual-sample-count ((ms-surface :surface-spec) :multi-sample)
	    num-samples-ref (multisample-data :num-samples-ref)]
	(when (not (= @num-samples-ref
		      actual-sample-count))
	  (dosync (ref-set num-samples-ref actual-sample-count)))
	  ;update ui to reflect reality which, due to differenes
	  ;in hardware, may not match what the user wanted.
	  (let [item (multisample-data :inspector-item)]
	    (when item
	      ((item :updater))))))))

(defn dmut-create-aa-drawable-fn[multisample-data drawable-fn]
  (let [frame-resize-data (ref {:resize-frame-count 0 :resize-frame-size [0 0]})
	fm (multisample-data :frame)
	rc-ref ((fm :win-data) :render-context-ref)
	aa-drawable-fn #(antialiasing-drawable-wrapper % rc-ref frame-resize-data multisample-data drawable-fn)]
    aa-drawable-fn))

(defn dmut-create-multisample-data
  "given an app frame, and callback that takes nothing and returns nothing,
create a multisample data item"
  [frame num-samples-changed]
  (let [num-samples-ref (ref :4)]
    (struct-map multisample-data
      :frame frame
      :num-samples-ref num-samples-ref
      :inspector-item (uii-create-list-inspector-item 
		       "Antialiasing: " ;item name
		       aa-choices-array ;choices
		       (fn [] @num-samples-ref) ;getter
		       (fn [val] 
			 (dosync (ref-set num-samples-ref val))
			 (num-samples-changed)) ;setter
		       (fn [item]
			 (if (= item :none)
			   "none"
			   (util-stringify (name item) "x")))))))