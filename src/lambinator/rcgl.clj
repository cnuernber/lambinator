(ns lambinator.rcgl
  (:use lambinator.rc lambinator.util
	lambinator.fs clojure.contrib.seq-utils
	clojure.contrib.except )
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
  :vbo_manager )

(defn create_render_context []
  (struct render_context 
	  (create_rcgl_glsl_manager)
	  (create_loading_system)
	  (create_vbo_manager)))
    
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

(defn append_to_ref_list [render_tasks_ref lmbda]
  (dosync (ref-set render_tasks_ref (conj @render_tasks_ref lmbda))))

(defn rcgl_delete_glsl_program[render_context_ref render_tasks_ref prog_name]
  (let [{ { programs_ref :programs_ref shaders_ref :shaders_ref } :glsl_manager } @render_context_ref]
    (append_to_ref_list render_tasks_ref #(delete_rcgl_glsl_program_and_shaders % programs_ref shaders_ref prog_name))))

;vbo type must be either :data or :index
;generator is a function that returns a sequence of numbers.  If they are float
;then you get a float buffer.  If they are bytes, then you get a byte buffer.
;Finally, if they are short, then you get a short buffer.  So pay attention
;when you are creating the sequence.
(defn rcgl_create_vbo [render_context_ref render_tasks_ref buf_name vbo_type generator]
  (let [{ { vbos_ref :vbos_ref } :vbo_manager } @render_context_ref]
    (append_to_ref_list render_tasks_ref #(create_vbo (. % getGL) vbos_ref buf_name vbo_type generator))))

(defn rcgl_delete_vbo [render_context_ref render_tasks_ref buf_name]
  (let [{ { vbos_ref :vbos_ref } :vbo_manager } @render_context_ref]
    (append_to_ref_list render_tasks_ref #(delete_vbo (. % getGL) vbos_ref buf_name ))))

(defn rcgl_delete_vbo [render_context_ref render_tasks_ref buf_name]
  (let [{ { vbos_ref :vbos_ref } :vbo_manager } @render_context_ref]
    (append_to_ref_list render_tasks_ref #(delete_vbo (. % getGL) vbos_ref buf_name ))))

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

;This is called when all of the resources were destroyed through nefarious means.
;The ones that can be regenerated will be.
;returns a new render context
(defn rcgl_resources_destroyed[drawable render_context]
  (let [{ { programs_ref :programs_ref shaders_ref :shaders_ref } :glsl_manager 
	  { vbos_ref :vbos_ref } :vbo_manager } render_context]
    (resources_released_reload_all_glsl_programs drawable programs_ref shaders_ref)
    (vbo_resources_destroyed (. drawable getGL) vbos_ref)))