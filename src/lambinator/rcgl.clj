(ns lambinator.rcgl
  (:use lambinator.rc lambinator.util
	lambinator.fs clojure.contrib.seq-utils )
  (:import (javax.media.opengl GL)
	   (java.io File)))


(load "rcgl_defs")
(load "rcgl_texture")
(load "rcgl_fbo")
(load "rcgl_glsl")

(defstruct render_context :surface_manager :texture_manager :glsl_manager :loading_system )

(defn create_render_context []
  (struct render_context 
	  (create_surface_manager)
	  (create_texture_manager)
	  (create_rcgl_glsl_manager)
	  (create_loading_system)))


;most of the functions below *have* to run in the gl thread.
;they are not threadsafe in the least.  Plus most of them take a gl interface
;which you should never access out of the display handler.

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

;Returns the glsl program mapped to this name
;or nil if the program doesn't exist or is invalid.
(defn rcgl_get_glsl_program[render_context prog_name]
  (let [{ { programs_ref :programs_ref } :glsl_manager } render_context
	program (@programs_ref prog_name)
	prog_valid (glsl_program_valid program)]
    (if prog_valid
      program
      nil)))

;This is called when all of the resources were destroyed through nefarious means.
;The ones that can be regenerated will be.
;returns a new render context
(defn rcgl_resources_destroyed[drawable render_context]
  (let [{ { programs_ref :programs_ref shaders_ref :shaders_ref } :glsl_manager } render_context]
    (resources_released_reload_all_glsl_programs drawable programs_ref shaders_ref)
    (assoc render_context :surface_mananger (create_surface_manager) :texture_manager (create_texture_manager))))
    

;OK to call outside render thread.  You can find the program
;via the name you passed in later.
;returns true if both files exist
;false if one of them does not.
;this takes the ref because it is a public, outside render thread function.
(defn rcgl_create_glsl_program[render_context_ref render_tasks_ref glslv_filename glslf_filename prog_name]
  (let [{ { programs_ref :programs_ref shaders_ref :shaders_ref } :glsl_manager
	  loading_system :loading_system } @render_context_ref
	glslv (get_full_path glslv_filename)
	glslf (get_full_path glslf_filename)]
    (if (and (file_exists? glslv)
	     (file_exists? glslf))
      (do
	(create_glsl_program_from_files programs_ref shaders_ref loading_system render_tasks_ref glslv glslf prog_name )
	true)
      false)))