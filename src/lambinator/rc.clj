(ns lambinator.rc
  (:use clojure.contrib.except lambinator.util))
;Render commands are pure data interfaces
;that contain the commands necessary to render
;a scene or some chunk of a scene.

;A render command begins with an integer that
;is arbitrarily mapped to more information.

;A series of render commands should completely 
;*completely* decide a render.  There can be nothing
;left out and I imagine there will be render commands
;that contain other render commands or that refer to
;subsets of other render commands.

;Some render commands specify a target; this will be an FBO,
;with FBO 0 meaning the target FBO.  FBO indexes are relative
;to the scene render command object.
;Thus a given render command is called the scene.  It contains a
;description of its designed FBO.

;You can build a render command list by iterating over a scene graph;
;but this concept covers more than scenegraphs and it covers aggregations
;of scenegraphs.

;Dealing with functional datastructures, it makes sense to define these
;items in terms of things that won't change render to render.  
;For example, they aren't going to hold a matrix, ever.  They will
;hold a handle to a matrix; thus when rendering the same scene multiple
;times during animation for the most part the render command list
;doesn't change; just the items being animated will have their
;slots updated.  

;all rcs need a type variable that indicates what they are.
;types are keywords.
;surface is a 2D slice that we can render to and bind as a texture
;Colors- floating point, 0.0-1.0 (greater than 1.0 is allowed for some
;operations)
(def rc-formats [:rgba :rgb :alpha :lum :lum-alpha])
(def rc-datatypes [:ubyte :ushort :float :half-float] ) ;values used for
(defstruct texture-spec :datatype :format :size)
(defstruct mipmapped-texture-spec :texture :mipmap-count)
;cube maps can have mipmapped textures specified as their
;textures.  I have never really tried this.
(defstruct cubemap-spec :top :bottom :left :right :front :back)
(defstruct color :r :g :b :a)

(defn create-texture-spec 
  [format type width height]
  (struct texture-spec type format [width height]))

(defn create-mipmapped-texture-spec [texture count]
     (struct mipmapped-texture-spec texture count))

(defn create-color [r g b a]
  (struct color r g b a))

(def renderbuffer-types [:color :depth])
(defstruct renderbuffer-data 
  :type ;the type, one of the above render buffer types
  :depth-bits ;number of bits; used only for depth buffers
  :color-datatype ;the color datatype, ubyte ushort half-float float
  :color-format ;format rbga rbg lum lum-alpha
  :use-texture) ;true if you want this renderbuffer to render to a texture.
(defmulti create-renderbuffer (fn [type & args] type))

(def surface-depth-bits [:16 :24 :32] ) ;values used for depth-bits

;create a depth renderbuffer specifying the bits from the above array
;and whether to use a texture
(defmethod create-renderbuffer :depth [type depth-bits use-texture] 
  (util-throw-if-item-missing depth-bits surface-depth-bits "depth-bits must be a surface-depth-bits: " depth-bits)
  (struct-map renderbuffer-data :type type :depth-bits depth-bits :use-texture use-texture))

;Create a color renderbuffer 
(defmethod create-renderbuffer :color [type datatype format use-texture]
  (util-throw-if-item-missing datatype rc-datatypes "datatype must be an rc datatype: " datatype)
  (util-throw-if-item-missing format rc-formats "format must be an rc format: " format)
  (struct-map renderbuffer-data 
    :type type ;color
    :color-datatype datatype ;datatype of the color buffer
    :color-format format     ;format of the color buffer
    :use-texture use-texture)) ;true if you want a texture from this buffer.


(def surface-attachment-points [:color0 :color1 :color2 :color3 :depth])
(def multisample-num-samples [:2 :4 :8 :16])
;a surface can have several attachments.  It *has* to have color0, but other than that
;it is up to you; if you want to have multiple render target support then you need to create
;FBOs that support this by filling in color0-colorn for each render target you want.
;If you specify a multisample buffer as one of the attachments, then you can't use
;texture attachments.  Thus a multi sample fbo setup is pretty particular and I considered
;making it a special surface type, but I figure it is better to just let users sort
;it all out ;).
(defstruct surface-spec :attachments :size :multi-sample)
(defn create-surface-spec 
  ([attachments width height multi-sample] 
     (when multi-sample
       (util-throw-if-item-missing multi-sample 
			      multisample-num-samples 
			      "datatype must be nil or multisample-num-samples: " multi-sample))
     (struct-map surface-spec 
       :attachments attachments
       :size [width height]
       :multi-sample multi-sample))
  ([attachments width height] (create-surface-spec attachments width height nil)))

;There are certain combinations that don't make any sense.
;like attaching a depth renderbuffer to a color attachment point.
;or attaching a multisample buffer to the depth attachment point.
;so just don't do it.
;returns a new sspec
(defn attach-renderbuffer [sspec attach-pt buffer]
  (assoc sspec :attachments 
	 (assoc (sspec :attachments) attach-pt buffer)))

(defn has-multi-sample[sspec]
  (not (nil? (sspec :multi-sample))))