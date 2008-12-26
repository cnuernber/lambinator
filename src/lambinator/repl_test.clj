(use 'lambinator.ui)
(use 'lambinator.rcgl)
(import '(java.io File))
(import '(javax.media.opengl GL))
(load-file "rcgl_glsl.clj")
(load-file "ui.clj")
(use 'lambinator.ui)
(use 'lambinator.rcgl)
(def fm (ui_create_app_frame "lemmings"))
(defn test_load_wave_glslv [drawable]
  (let [gl (. drawable getGL)
	file (File. "../data/glsl/wave.glslv")]
    (test_create_glsl_script_from_file gl (. file getCanonicalPath))))
(defn test_load_wave_glslf [drawable]
  (let [gl (. drawable getGL)
	file (File. "../data/glsl/wave.glslf")]
    (test_create_glsl_script_from_file gl (. file getCanonicalPath))))

(defn test_create_proggy [drawable]
  (let [vs (test_load_wave_glslv drawable)
	fs (test_load_wave_glslf drawable)
	gl (. drawable getGL)]
    (create_glsl_program gl vs fs "proggy")))

