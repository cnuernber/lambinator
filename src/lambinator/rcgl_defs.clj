(in-ns 'lambinator.rcgl)
;Simple render context implementation that binds the
;render commands to sets of opengl calls

(defn create_context_texture [texture_spec gl_handle]
  (struct context_texture texture_spec gl_handle))

(defn create_texture_manager [textures]
  (struct texture_manager textures))

(defn create_context_surface [surface_spec relative_index 
			      render_width render_height texture_index 
			      gl_handle]
  (struct context_surface surface_spec 
	  relative_index (list render_width render_height) 
	  texture_index gl_handle))

(defn create_surface_manager [all_surfaces unused_surfaces]
  (struct surface_manager all_surfaces unused_surfaces))

(defn create_render_context [surface_manager]
  (struct render_context surface_manager))

;this is complex.  You want to return the smallest
;surface where both width,height are >= desired
;If you can't get that, you want the one that will
;require the fewest more bytes allocated on the
;card.
;Takes a vector of surfaces, an index of first surface to compare
;an index of the second surface to compare,
;and the desired texture_spec
;This assumes that you have already filtered the possibilities by
;the ones that have matching specification such as format and texture_type
(defn surface_matches_better [all_surfaces ctx_surface_idx1 ctx_surface_idx2 desired]
  ;ensure valid surfaces are passed in
  (if (== -1 ctx_surface_idx1)
    ctx_surface_idx2
    (if (== -1 ctx_surface_idx2)
      ctx_surface_idx1
      (let [ctx_surface1 (all_surfaces ctx_surface_idx1)
	    ctx_surface2 (all_surfaces ctx_surface_idx2)
	    surface1 (ctx_surface1 :surface_spec)
	    surface2 (ctx_surface2 :surface_spec)
	    required1 (bytes_required surface1 desired)
	    required2 (bytes_required surface2 desired)]
	(if (and (== required1 0)
		 (== required2 0)) ;desired would fit in either 1 or 2
	  (if (<= (overdraw surface1 desired)
		  (overdraw surface2 desired)) ;return the one with the least overdraw
	    ctx_surface_idx1
	    ctx_surface_idx2)
	  (if (== required1 0) ;desired would fit in s1
	    ctx_surface_idx1
	    (if (== required2 0) ;desired would fit in s2
	      ctx_surface_idx2
	      (if (<= required1 required2) ;return the least number of types required
		ctx_surface_idx1
		ctx_surface_idx2))))))))

;Allocator must return a context surface if requested
;returns a list of new surface manager along with new context surface index
;re-allocator must take the surface spec, the list of all surfaces, and an index
;that *may* need to be re-allocated if its width or height is less than
;specified in the surface spec. IT should return a new list of all surfaces
(defn allocate_surface [allocator reallocator surface_manager surface_spec]
  (let [all_surfaces (surface_manager :all_surfaces)
	unused_surfaces (surface_manager :unused_surfaces)
	possible_unused (filter 
			 (fn [idx]
			   (let [ctx_src (all_surfaces idx)
				 ctx_spec (ctx_src :surface_spec)]
			     (surface_details_match ctx_spec surface_spec))))
	[best desired] (reduce (fn [[best_so_far desired] idx ]
				 (let [ctx_src (all_surfaces idx)
				       ctx_spec (ctx_src :surface_spec)]
				   (list 
				    (surface_matches_better all_surfaces best_so_far 
							    ctx_spec desired)
				    desired)))
			       [-1 surface_spec] 
			       possible_unused )]
    (if (== best -1)
      (let [retval (count all_surfaces)]
	(list 
	 (create_surface_manager (conj all_surfaces (allocator surface_spec)) 
				 unused_surfaces)
	 retval))
      (list 
       (create_surface_manager 
	(reallocator all_surfaces best surface_spec) 
	(filter #(== best %) unused_surfaces))
       best))))

;Returns the index to the surface manager's unused list.
;Returns a new surface manager
(defn return_surface [surface_manager index]
  (create_surface_manager 
   (surface_manager :all_surfaces) 
   (conj (surface_manager :unused_surfaces) index)))

(defmulti perform_render_command :render_command_type)

(defmethod perform_render_command :default [cmd context]
  (println "unrecognized render command")
  context)
(defmethod perform_render_command :scene_render_command [cmd context]
  (println "rendering the scene")
  context)