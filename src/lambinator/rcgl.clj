(ns lambinator.rcgl
  (:use lambinator.rc)
  (:import (javax.media.opengl GL)))

(defstruct context_texture :texture_spec :gl_handle)
(defstruct texture_manager :textures )
;surfaces is a vector of all the known surfaces
;unused is a linked list of the unused surfaces
;render size may be <= surface size
(defstruct context_surface :surface_spec :relative_index :render_size 
	   :texture_index :gl_handle)

(defstruct surface_manager :all_surfaces :unused_surfaces)

(defstruct render_context :surface_manager :texture_manager)

(load "rcgl_defs")