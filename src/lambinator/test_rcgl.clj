(in-ns 'lambinator.test)

(defn create_surfaces_test_data []
  (apply 
   vector	
   (mapcat 
    (fn [size]
      [
       (create_context_surface 
	(create_surface_spec 16 false 
			     (create_texture_spec :rgba :ubyte size size))
	0 1)
       (create_context_surface
	(create_surface_spec 16 false 
			     (create_texture_spec :rgb :ubyte size size))
	0 1)])
    [100 200 300 400])))

(defn create_test_surfaces_manager []
  (create_surface_manager (create_surfaces_test_data) 
			  (take 8 (iterate #(+ % 1) 0))))

(deftest test_filter_possible_context_surfaces []
	 (let [mgr (create_test_surfaces_manager)
	       all_surfaces (mgr :all_surfaces)
	       unused_surfaces (mgr :unused_surfaces)]
	   (is (= 
		(filter_possible_context_surfaces 
		 all_surfaces unused_surfaces
		 ((all_surfaces 0) :surface_spec))
		(list 0 2 4 6)))
	   (is (= 
		(filter_possible_context_surfaces 
		 all_surfaces unused_surfaces
		 ((all_surfaces 1) :surface_spec))
		(list 1 3 5 7)))
	   (is (=
		(filter_possible_context_surfaces 
		 all_surfaces unused_surfaces
		 (create_surface_spec 
		  16 false 
		  (create_texture_spec :lum_alpha :ubyte 300 300)))
		nil))
	   ;change one of the surfaces to be unused
	   (let [old_surface (all_surfaces 4)
		 new_surface (create_context_surface 
			      (old_surface :surface_spec)
			      (old_surface :texture_index)
			      0)
		 all_surfaces (assoc all_surfaces 4 new_surface)]
	     (is (= 
		  (filter_possible_context_surfaces 
		   all_surfaces unused_surfaces
		   ((all_surfaces 0) :surface_spec))
		  (list 0 2 6))))
	   ))

(deftest test_context_surface_matches_better []
	 (let [mgr (create_test_surfaces_manager)
	       all_surfaces (mgr :all_surfaces)
	       unused_surfaces (mgr :unused_surfaces)]
	   ;test edge cases of -1 first
	   (is (=
		(context_surface_matches_better
		 all_surfaces 
		 -1 
		 0
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		0))
	   (is (=
		(context_surface_matches_better
		 all_surfaces 
		 0 
		 -1
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		0))
	   (is (= 
		(context_surface_matches_better
		 all_surfaces 
		 0 
		 4
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		4))
	   (is (= 
		(context_surface_matches_better
		 all_surfaces 
		 4 
		 6
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		4))
	   (is (= 
		(context_surface_matches_better
		 all_surfaces 
		 2 
		 6
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		6))
	   (is (= 
		(context_surface_matches_better
		 all_surfaces 
		 0 
		 2
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		2))))
	   
(deftest test_find_best_context_surface []
	 (let [mgr (create_test_surfaces_manager)
	       all_surfaces (mgr :all_surfaces)
	       unused (mgr :unused_surfaces)]
	   (is (=
		(find_best_context_surface
		 all_surfaces 
		 nil
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		-1))
	   (is (=
		(find_best_context_surface
		 all_surfaces 
		 unused
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		5))
	   (is (=
		(find_best_context_surface
		 all_surfaces 
		 (take 3 unused)
		 (create_surface_spec 16 false 
				      (create_texture_spec 
				       :rgb :ubyte 300 300)))
		1))
	   ))

(defn do_test_render_context_fbo []
  (let [gl (create_gl_mock_object)
	rc (create_render_context)
	sspec (create_surface_spec 
	       :24 false
	       (create_texture_spec :rgba :ubyte 400 300))
	[rc idx] (rcgl_allocate_fbo gl rc sspec)
	surface_manager (rc :surface_manager)
	texture_manager (rc :texture_manager)]
    (is (== idx 0))
    (is (== (((surface_manager :all_surfaces) 0) :gl_handle)
	    1))
    (is (== (((texture_manager :textures) 0) :gl_handle)
	    1))
    (is (= nil (first (surface_manager :unused_surfaces))))
    (let [rc (rcgl_mark_fbo_unused gl rc 0)
	  surface_manager1 (rc :surface_manager)
	  texture_manager1 (rc :texture_manager)]
      (is (== (first (surface_manager1 :unused_surfaces)) 0))
      (let [sspec (create_surface_spec
		   :24 false
		   (create_texture_spec :rgba :ubyte 800 600))
	    [rc idx] (rcgl_allocate_fbo gl rc sspec) ;force a reallocation
	    surface_manager2 (rc :surface_manager)
	    texture_manager2 (rc :texture_manager)]
	(is (== (((surface_manager2 :all_surfaces) 0) :gl_handle)
		1))
	(is (== (((texture_manager2 :textures) 0) :gl_handle)
		1))
	(is (= nil (first (surface_manager2 :unused_surfaces))))
	(let [rc (rcgl_destroy_fbo gl rc 0)
	      surface_manager3 (rc :surface_manager)
	      texture_manager3 (rc :texture_manager)]
	  (is (not (== (((surface_manager3 :all_surfaces) 0) :gl_handle)
		       1)))
	  (is (== (((texture_manager3 :textures) 0) :gl_handle)
		   1))
	  (is (not (surface_manager3 :unused_surfaces)))
	  (let [[rc idx] (rcgl_allocate_fbo gl rc sspec)]
	    (is (== idx 0))
	    (let [rc (rcgl_mark_fbo_unused gl rc 0)
		  [rc idx] (rcgl_allocate_fbo gl rc sspec)]
	      (is (== idx 0))
	      (is (== (((surface_manager :all_surfaces) 0) :gl_handle)
		      1))
	      (is (== (((texture_manager :textures) 0) :gl_handle)
		      1))
	      (is (= nil (first (surface_manager :unused_surfaces))))
	      )))))))

(deftest test_render_context_fbo []
	 (do_test_render_context_fbo))