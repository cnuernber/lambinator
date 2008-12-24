(in-ns 'lambinator.test)

(deftest test_util_find_next_matching_index []
  (is (= [0 [true]]
	   (util_find_next_matching_index [] identity (fn [] true))))
  (is (= [3 [false false false true false]]
	   (util_find_next_matching_index 
	    [false false false true false] identity (fn [] true))))
  (is (= [5 [false false false false false true]]
	   (util_find_next_matching_index 
	    [false false false false false] identity (fn [] true)))))