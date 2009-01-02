(ns lambinator.rcgl
  (:use lambinator.rc lambinator.util
	lambinator.fs clojure.contrib.seq-utils
	clojure.contrib.except
	lambinator.log)
  (:import (javax.media.opengl GL)
	   (java.io File)))

(load "rcgl_defs")
(load "rcgl_texture")
(load "rcgl_fbo")
(load "rcgl_glsl")
(load "rcgl_vbo")

(defstruct render_context  
  :glsl_manager 
  :loading_system
  :vbo_manager
  :surfaces_ref
  :logger_ref)

(defn create_render_context [logger_ref]
  (struct render_context 
	  (create_rcgl_glsl_manager)
	  (create_loading_system)
	  (create_vbo_manager)
	  (ref {})
	  logger_ref))
    
;OK to call outside render thread.  You can find the program
;via the name you passed in later.
;returns true if both files exist
;false if one of them does not.
;this takes the ref because it is a public, outside render thread function.
(defn rcgl_create_glsl_program[render_context_ref render_tasks_ref glslv_filename glslf_filename prog_name]
  (let [{ { programs_ref :programs_ref shaders_ref :shaders_ref } :glsl_manager
	  loading_system :loading_system 
	  logger_ref :logger_ref } @render_context_ref
	glslv (get_full_path glslv_filename)
	glslf (get_full_path glslf_filename)]
    (if (and (file_exists? glslv)
	     (file_exists? glslf))
      (do
	(create_glsl_program_from_files logger_ref programs_ref shaders_ref loading_system render_tasks_ref glslv glslf prog_name )
	true)
      false)))

(defn append_to_ref_list [render_tasks_ref lmbda]
  (dosync (ref-set render_tasks_ref (conj @render_tasks_ref lmbda))))

(defn rcgl_delete_glsl_program[render_context_ref render_tasks_ref prog_name]
  (let [{ { programs_ref :programs_ref shaders_ref :shaders_ref } :glsl_manager 
	  logger_ref :logger_ref } @render_context_ref]
    (append_to_ref_list render_tasks_ref #(delete_rcgl_glsl_program_and_shaders logger_ref % programs_ref shaders_ref prog_name))))

(defn rcgl_set_glsl_uniforms[render_context gl var_pair_seq rcgl_glsl_program]
  (let [logger_ref (render_context :logger_ref)]
    (set_glsl_prog_uniforms logger_ref gl var_pair_seq rcgl_glsl_program)))

;vbo type must be either :data or :index
;generator is a function that returns a sequence of numbers.  If they are float
;then you get a float buffer.  If they are bytes, then you get a byte buffer.
;Finally, if they are short, then you get a short buffer.  So pay attention
;when you are creating the sequence.
(defn rcgl_create_vbo [render_context_ref render_tasks_ref buf_name vbo_type generator]
  (let [{ { vbos_ref :vbos_ref } :vbo_manager  
	  logger_ref :logger_ref } @render_context_ref]
    (append_to_ref_list render_tasks_ref #(create_vbo logger_ref (. % getGL) vbos_ref buf_name vbo_type generator))))

(defn rcgl_delete_vbo [render_context_ref render_tasks_ref buf_name]
  (let [{ { vbos_ref :vbos_ref } :vbo_manager  
	  logger_ref :logger_ref } @render_context_ref]
    (append_to_ref_list render_tasks_ref #(delete_vbo logger_ref (. % getGL) vbos_ref buf_name ))))

;Functions below are query functions of the render context.
;They take a non-ref'd context as they don't change the context
(defn rcgl_get_vbo [render_context name]
  (let [{ { vbos_ref :vbos_ref } :vbo_manager } render_context
	retval (@vbos_ref name)]
    (if (gl_vbo_valid retval)
      retval
      nil)))

;Returns the glsl program mapped to this name
;or nil if the program doesn't exist or is invalid.
(defn rcgl_get_glsl_program[render_context prog_name]
  (let [{ { programs_ref :programs_ref } :glsl_manager } render_context
	program (@programs_ref prog_name)
	prog_valid (glsl_program_valid program)]
    (if prog_valid
      program
      nil)))


;unlike vbos or gl programs, the system is completely capable of
;creating surfaces during the render process.  Plus, there is never a good reason
;to pass processing off to another thread; you will always just block at the card
;trying to create them.
(defn rcgl_create_context_surface[render_context_ref render_tasks_ref sspec name]
  (append_to_ref_list render_tasks_ref 
		      #(create_named_context_surface (@render_context_ref :logger_ref) (. % getGL) (@render_context_ref :surfaces_ref) sspec name)))

;Create a context surface using a list of fallbacks
(defn rcgl_create_context_surface_seq[render_context_ref render_tasks_ref sspec_seq name]
  (append_to_ref_list render_tasks_ref 
		      #(create_named_context_surface_seq (@render_context_ref :logger_ref) (. % getGL) (@render_context_ref :surfaces_ref) sspec_seq name)))


;only runs if the surface exists already
(defn rcgl_update_context_surface[render_context_ref render_tasks_ref name width height]
  (append_to_ref_list render_tasks_ref 
		      #(update_named_context_surface (@render_context_ref :logger_ref) (. % getGL) (@render_context_ref :surfaces_ref) name width height)))

(defn rcgl_delete_context_surface[render_context_ref render_tasks_ref name]
  (append_to_ref_list render_tasks_ref 
		      #(delete_named_context_surface (@render_context_ref :logger_ref) (. % getGL) (@render_context_ref :surfaces_ref) name)))

;this is meant to be called from within the render thread
(defn rcgl_get_or_create_context_surface[render_context_ref gl sspec name]
  (get_or_create_context_surface (@render_context_ref :logger_ref) gl (@render_context_ref :surfaces_ref) sspec name))

(defn rcgl_get_context_surface[render_context name]
  (let [surfaces_ref (render_context :surfaces_ref)
	retval (@surfaces_ref name)]
    (if (context_surface_valid_for_render retval)
      retval
      nil)))
	

;This is called when all of the resources were destroyed through nefarious means.
;The ones that can be regenerated will be.
;returns a new render context
(defn rcgl_resources_destroyed[drawable render_context]
  (let [{ { programs_ref :programs_ref shaders_ref :shaders_ref } :glsl_manager 
	  { vbos_ref :vbos_ref } :vbo_manager 
	  surfaces_ref :surfaces_ref 
	  logger_ref :logger_ref } render_context ]
    (resources_released_reload_all_glsl_programs logger_ref drawable programs_ref shaders_ref)
    (vbo_resources_destroyed logger_ref (. drawable getGL) vbos_ref)
    (context_surfaces_destroyed logger_ref (. drawable getGL) surfaces_ref))
    render_context)