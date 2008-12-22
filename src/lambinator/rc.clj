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
(def texture_types [:ubyte :short :float :half_float] ) ;values used for
(def surface_depth_types [:16 :24 :32 :none] ) ;values used for depth_bits
(defstruct texture_spec :texture_type :format :size)
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

(load "rc_defs")