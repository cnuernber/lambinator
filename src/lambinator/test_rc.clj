(in-ns 'lambinator.test)

(deftest texture_details_match_test []
	 (is (not (texture_details_match 
		   (create_texture_spec :rgba :ubyte 400 30 )
		   (create_texture_spec :rgb :ubyte 400 300 ))))
	 (is (texture_details_match
	      (create_texture_spec :rgba :float 500 500)
	      (create_texture_spec :rgba :float 600 800))))

(deftest surface_details_match_test []
	 (let [ts (create_texture_spec :rgba :ubyte 400 300)]
	   (is (not (surface_details_match 
		     (create_surface_spec 16 false ts)
		     (create_surface_spec 24 false ts))))
	   (is (create_surface_spec 16 false ts)
	       (create_surface_spec 16 false ts))))

(defn test_create_surface_width_height [width height]
  (create_surface_spec 16 false 
		       (create_texture_spec :rgba :ubyte width height)))

(deftest bytes_required_test []
	 (is (== 0 (bytes_required
		  (test_create_surface_width_height 500 500)
		  (test_create_surface_width_height 400 400))))
	 (is (> (bytes_required
		 (test_create_surface_width_height 400 400)
		 (test_create_surface_width_height 500 500))
		0)))
	 