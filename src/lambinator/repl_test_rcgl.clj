(use 'lambinator.rcgl)
(import '(java.io File))

(defn create_test_data [fnames]
  (map (fn [fname]
	 (. (File. fname) getCanonicalPath))
       fnames))

(def rcgl_shaders (create_test_data 
		   ["../data/glsl/wave.glslv"
		    "../data/glsl/wave.glslf"
		    "../data/glsl/test1.glslv"
		    "../data/glsl/test2.glslf"
		    "../data/glsl/test3.glslv"
		    "../data/glsl/invalid.glslf"
		    "../data/glsl/wontlink.glslf"]))