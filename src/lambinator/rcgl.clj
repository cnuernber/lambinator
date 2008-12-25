(ns lambinator.rcgl
  (:use lambinator.rc lambinator.util)
  (:import (javax.media.opengl GL)))

(defstruct context_texture :texture_spec :gl_handle)
(defstruct texture_manager :textures )
;surfaces is a vector of all the known surfaces
;unused is a linked list of the unused surfaces
;render size may be <= surface size
(defstruct context_surface :surface_spec :texture_index :gl_handle :framebuffer_complete )

(defstruct surface_manager :all_surfaces :unused_surfaces)

(defstruct render_context :surface_manager :texture_manager)

(load "rcgl_defs")
(load "rcgl_texture")
(load "rcgl_fbo")

(defn create_render_context []
  (struct render_context 
	  (struct surface_manager [] nil)
	  (struct texture_manager [] )))

;takes a gl, a render context, and a surface spec.
;returns a new render context and the index of the allocated fbo 
;called a context_surface.
(defn rcgl_allocate_fbo[gl render_context surface_spec]
  (let [surfaces (render_context :surface_manager)
	textures (render_context :texture_manager)
	result (allocate_context_surface gl surfaces (textures :textures) surface_spec)
	[new_surfaces new_textures_array result_index] result
	new_rc (assoc render_context 
		 :surface_manager new_surfaces 
		 :texture_manager (assoc textures :textures new_textures_array))]
    [new_rc result_index]))

;destroys the given FBO, returns a new render context
;destroying the fbo destroys the texture allocated
;for the FBO as well
(defn rcgl_destroy_fbo[gl render_context surface_index]
  (let [old_surface_manager (render_context :surface_manager)
	old_texture_manager (render_context :texture_manager)
	old_all_surfaces (old_surface_manager :all_surfaces)
	old_textures (old_texture_manager :textures)
	[new_all_surfaces textures] (release_context_surface gl surface_index old_all_surfaces old_textures)
	surface_manager (assoc old_surface_manager :all_surfaces new_all_surfaces)
	texture_manager (assoc old_texture_manager :textures textures)]
    (assoc render_context :surface_manager surface_manager :texture_manager texture_manager)))

;much *much* cheaper than destroying an fbo is just marking it
;as unused.  This is ideal but it doesn't return memory to
;the card.
(defn rcgl_mark_fbo_unused[gl render_context surface_index]
  (let [old_surface_manager (render_context :surface_manager)
	unused_surfaces (old_surface_manager :unused_surfaces)
	surface_manager (assoc old_surface_manager :unused_surfaces (conj unused_surfaces surface_index))]
    (assoc render_context :surface_manager surface_manager)))

(defn rcgl_context_surface_size[render_context surface_index]
  (let [{ { { { { { size :size }  ;yeah, wassup with my binding forms now, beeatch
		  :texture_spec } 
		:surface_spec } 
	      surface_index } 
	    :all_surfaces } 
	  :surface_manager } render_context ]
    size ))
	
	
  
    
  