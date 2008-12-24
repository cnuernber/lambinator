(in-ns 'lambinator.test)

(defn do_test_mock_gl_gen_items [gen_func release_func]
  (is (= 
       (allocate_gl_item gen_func )
       1))
  (is (= 
       (allocate_gl_item gen_func)
       2))
  (release_gl_item release_func 1)
  (is (= 
       (allocate_gl_item gen_func)
       1))
  (is (= 
       (allocate_gl_item gen_func)
       3)))

(defn do_test_mock_gl_gen_textures [gl]
  (do_test_mock_gl_gen_items 
   (fn [count args offset] (. gl glGenTextures count args offset))
   (fn [count args offset] (. gl glDeleteTextures count args offset))))

(deftest test_mock_gl_gen_textures 
  []
  (let [gl (create_gl_mock_object)]
    (do_test_mock_gl_gen_textures gl)))
    
    
	 