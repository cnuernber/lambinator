(ns lambinator.mockgl
  (:import (javax.media.opengl GL))
  (:use lambinator.rcgl lambinator.util))


(defstruct gl_mock_context
  :textures
  :fbos
  :renderbuffers)

;create an empty gl context with nothing in it.
(defn create_gl_mock_context []
  (struct gl_mock_context [true] [true] [true]))

;find a false item and push a new one or 
(defn mock_find_next_unused_index[bool_vector]
  (util_find_next_matching_index bool_vector not (fn [] true)))

(defn mock_gl_gen_items[bool_vector number result_array offset]
  (reduce (fn [bool_vector index]
	    (let [[item_index new_vector] (mock_find_next_unused_index bool_vector)]
	      (aset result_array (+ offset index) item_index)
	      (assoc new_vector item_index true)))
	  bool_vector 
	  (take number (iterate inc 0))))

(defn mock_gl_release_items[bool_vector number items_array offset]
  (reduce (fn [bool_vector index]
	    (assoc bool_vector (aget items_array (+ offset index)) false))
	  bool_vector 
	  (take number (iterate inc 0))))

(defn mock_gl_change_context_item[context_ref key number items_array offset change_fn]
  (dosync
   (let [context @context_ref
	 new_context (assoc context key (change_fn (context key) number items_array offset))]
     (ref-set context_ref new_context))))

(defn mock_gl_gen_context_item [context_ref key number items_array offset]
  (mock_gl_change_context_item context_ref key number items_array offset mock_gl_gen_items)
  nil)

(defn mock_gl_release_context_item [context_ref key number items_array offset]
  (mock_gl_change_context_item context_ref key number items_array offset mock_gl_release_items)
  nil)

(defn create_gl_mock_object 
  ( [context]
      (proxy [Object GL] []
	(glGenTextures [count args offset] (mock_gl_gen_context_item context :textures count args offset))
	(glDeleteTextures [count args offset] (mock_gl_release_context_item context :textures count args offset))
	(glGenFramebuffersEXT [count args offset] (mock_gl_gen_context_item context :fbos count args offset))
	(glDeleteFramebuffersEXT [count args offset] (mock_gl_release_context_item context :fbos count args offset))
	(glGenRenderbuffersEXT [count args offset] (mock_gl_gen_context_item context :renderbuffers count args offset))
	(glDeleteRenderbuffersEXT [count args offset] (mock_gl_release_context_item context :renderbuffers count args offset))
					;stubs for now
	(glBindFramebufferEXT [target hdl] nil)
	(glBindTexture [target hdl] nil)
	(glTexImage2D [target mip_level int_format width height border ext_format ext_type buffer] nil)
	(glFramebufferTexture2DEXT [target attachment_point tex_target tex_handle mip_level] nil)
	(glBindRenderbufferEXT [target buf_hdl] nil)
	(glRenderbufferStorageEXT [target depth_constant width height] nil)
	(glFramebufferRenderbufferEXT [target attach_pt attach_type buf_hdl] nil)
	(glCheckFramebufferStatusEXT [target] GL/GL_FRAMEBUFFER_COMPLETE_EXT)
	(get_gl_mock_context [] @context)))

  ( []
      (create_gl_mock_object (ref (create_gl_mock_context)))))
      
				
       
		     