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
(def rc_formats [:rgba :rgb :alpha :lum :lum_alpha])
(def rc_datatypes [:ubyte :ushort :float :half_float] ) ;values used for
(defstruct texture_spec :datatype :format :size)
(defstruct mipmapped_texture_spec :texture :mipmap_count)
;cube maps can have mipmapped textures specified as their
;textures.  I have never really tried this.
(defstruct cubemap_spec :top :bottom :left :right :front :back)
(defstruct color :r :g :b :a)


;Now we start with render command definitions
;The scene will assume that its surface spec is allocated
;and set as relative FBO index 0.
;A scene is an implicit allocate and release of surface 0.
;furthermore, when a scene is finished all relative surfaces
;still held, if any other than relative surface 0,
;should be released.
(def render_command_types 
     [:scene_render_command
      :allocate_surface_render_command 
      :release_surface_render_command])
(defstruct scene_render_command :render_command_type :surface_spec :clear_color )

(defn create_texture_spec [format type width height]
  (struct texture_spec type format [width height]))

(defn create_mipmapped_texture_spec [texture count]
     (struct mipmapped_texture_spec texture count))

(defn create_color [r g b a]
  (struct color r g b a))


;do details; type and format match
(defn texture_details_match [texture1 texture2]
  (and (= (texture1 :datatype)
	   (texture2 :datatype))
       (= (texture1 :format)
	   (texture2 :format))))

;ignoring width and height, could you substitute
;surface1 for surface2
(defn surface_details_match [surface1 surface2]
  (and (= (surface1 :attachments)
	  (surface2 :attachments))
       (= (surface1 :multi_sample)
	  (surface2 :multi_sample))))

;how many bytes would you have to allocate to
;fit desired in surface
;Assume you aren't going to shrink a surface
(defn bytes_required [surface desired]
  (let [[sw sh] ( surface :size)
	[dw dh] (desired :size)
	max (fn [s d] (if (> d s) d s))]
    (if (and (>= sw dw)
	     (>= sh dh))
      0
      (- (* (max sw dw) (max sh dh)) (* sh sw)))))

(defn overdraw [surface desired]
  (let [[sw sh] (surface :size)
	[dw dh] (desired :size)]
    (- (* sw sh) (* dw dh))))


(def renderbuffer_types [:color :depth])
(defstruct renderbuffer_data 
  :type ;the type, one of the above render buffer types
  :depth_bits ;number of bits; used only for depth buffers
  :color_datatype ;the color datatype, ubyte ushort half_float float
  :color_format ;format rbga rbg lum lum_alpha
  :use_texture) ;true if you want this renderbuffer to render to a texture.
(defmulti create_renderbuffer (fn [type & args] type))

(def surface_depth_bits [:16 :24 :32] ) ;values used for depth_bits

;create a depth renderbuffer specifying the bits from the above array
;and whether to use a texture
(defmethod create_renderbuffer :depth [type depth_bits use_texture] 
  (throw_if_item_missing depth_bits surface_depth_bits "depth_bits must be a surface_depth_bits: " depth_bits)
  (struct-map renderbuffer_data :type type :depth_bits depth_bits :use_texture use_texture))

;Create a color renderbuffer 
(defmethod create_renderbuffer :color [type datatype format use_texture]
  (throw_if_item_missing datatype rc_datatypes "datatype must be an rc datatype: " datatype)
  (throw_if_item_missing format rc_formats "format must be an rc format: " format)
  (struct-map renderbuffer_data 
    :type type ;color
    :color_datatype datatype ;datatype of the color buffer
    :color_format format     ;format of the color buffer
    :use_texture use_texture)) ;true if you want a texture from this buffer.


(def surface_attachment_points [:color0 :color1 :color2 :color3 :depthl])
(def multisample_num_samples [:2 :4 :8 :16])
;a surface can have several attachments.  It *has* to have color0, but other than that
;it is up to you; if you want to have multiple render target support then you need to create
;FBOs that support this by filling in color0-colorn for each render target you want.
;If you specify a multisample buffer as one of the attachments, then you can't use
;texture attachments.  Thus a multi sample fbo setup is pretty particular and I considered
;making it a special surface type, but I figure it is better to just let users sort
;it all out ;).
(defstruct surface_spec :attachments :size :multi_sample)
(defn create_surface_spec 
  ([attachments width height multi_sample] 
     (when multi_sample
       (throw_if_item_missing multi_sample 
			      multisample_num_samples 
			      "datatype must be multisample_num_samples: " multi_sample))
     (struct-map surface_spec 
       :attachments attachments
       :size [width height]
       :multi_sample multi_sample))
  ([attachments width height] (create_surface_spec attachments width height nil)))

;There are certain combinations that don't make any sense.
;like attaching a depth renderbuffer to a color attachment point.
;or attaching a multisample buffer to the depth attachment point.
;so just don't do it.
;returns a new sspec
(defn attach_renderbuffer [sspec attach_pt buffer]
  (assoc sspec :attachments 
	 (assoc (sspec :attachments) attach_pt buffer)))

(defn has_multi_sample[sspec]
  (not (nil? (sspec :multi_sample))))