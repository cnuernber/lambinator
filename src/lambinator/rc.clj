(ns lambinator.rc)
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
(def texture_formats [:rgba :rgb :alpha :lum :lum_alpha])
(def texture_datatypes [:ubyte :ushort :float :half_float] ) ;values used for
(def surface_depth_types [:16 :24 :32 :none] ) ;values used for depth_bits
(defstruct texture_spec :datatype :format :size)
(defstruct mipmapped_texture_spec :texture :mipmap_count)
;cube maps can have mipmapped textures specified as their
;textures.  I have never really tried this.
(defstruct cubemap_spec :top :bottom :left :right :front :back)
(defstruct color :r :g :b :a)
;A surface is usually going to be an FBO object
(defstruct surface_spec :depth_bits :stencil :texture_spec )


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
;you can request a surface that is larger or smaller
;than the world you intend to render to.  This results
;in scaling the result up or down.  I wouldn't request
;a size that has a different aspect ratio at this moment.
(defstruct allocate_surface_render_command :render_command_type :relative_index 
	   :surface_spec :render_size :clear_color )
(defstruct release_surface_render_command :render_command_type :relative_index)

(defn create_texture_spec [format type width height]
  (struct texture_spec type format [width height]))

(defn create_mipmapped_texture_spec [texture count]
     (struct mipmapped_texture_spec texture count))

(defn create_color [r g b a]
  (struct color r g b a))

(defn create_surface_spec [depth_bits stencil texture_spec]
  (struct surface_spec depth_bits stencil texture_spec))

(defn create_scene_render_command [surface_spec clear_color]
  (struct scene_render_command :scene_render_command surface_spec clear_color))


;do details; type and format match
(defn texture_details_match [texture1 texture2]
  (and (= (texture1 :datatype)
	   (texture2 :datatype))
       (= (texture1 :format)
	   (texture2 :format))))

;ignoring width and height, could you substitute
;surface1 for surface2
(defn surface_details_match [surface1 surface2]
  (and (texture_details_match (surface1 :texture_spec)
			      (surface2 :texture_spec))
       (= (surface1 :depth_bits)
	   (surface2 :depth_bits))
       (= (surface1 :stencil)
	   (surface2 :stencil))))

;how many bytes would you have to allocate to
;fit desired in surface
;Assume you aren't going to shrink a surface
(defn bytes_required [surface desired]
  (let [[sw sh] ((surface :texture_spec) :size)
	[dw dh] ((desired :texture_spec) :size)
	max (fn [s d] (if (> d s) d s))]
    (if (and (>= sw dw)
	     (>= sh dh))
      0
      (- (* (max sw dw) (max sh dh)) (* sh sw)))))

(defn overdraw [surface desired]
  (let [[sw sh] ((surface :texture_spec) :size)
	[dw dh] ((desired :texture_spec) :size)]
    (- (* sw sh) (* dw dh))))
