(ns lambinator.rcgl
  (:use lambinator.rc lambinator.util
	lambinator.fs clojure.contrib.seq-utils
	clojure.contrib.except
	lambinator.log lambinator.rcgl.util
	lambinator.rcgl.fbo
	lambinator.rcgl.glsl
	lambinator.rcgl.vbo
	lambinator.rcgl.texture
	lambinator.rcgl.image)
  (:import (javax.media.opengl GL)
	   (java.io File)))

(defstruct render-context  
  :glsl-programs
  :glsl-shaders
  :loading-system
  :vbo-manager
  :surfaces-ref
  :logger-ref
  :texture-map-ref
  :image-map-ref)

(defn create-render-context [logger-ref]
  (struct-map render-context 
    :glsl-programs-ref (ref {})
    :glsl-shaders-ref (ref {})
    :loading-system (create-loading-system)
    :vbos-ref (ref {})
    :surfaces-ref (ref {})
    :logger-ref logger-ref
    :texture-map-ref (ref {})
    :image-map-ref (ref {})))
    
;OK to call outside render thread.  You can find the program
;via the name you passed in later.
;returns true if both files exist
;false if one of them does not.
;this takes the ref because it is a public, outside render thread function.
(defn rcgl-create-glsl-program[render-context-ref render-tasks-ref glslv-filename glslf-filename prog-name]
  (let [{ programs-ref :glsl-programs-ref shaders-ref :glsl-shaders-ref
	  loading-system :loading-system 
	  logger-ref :logger-ref } @render-context-ref
       
	  glslv (fs-get-full-path glslv-filename)
	  glslf (fs-get-full-path glslf-filename)]
    (if (and (fs-file-or-resource-exists? glslv)
	     (fs-file-or-resource-exists? glslf))
      (do
	(rcglg-create-program-from-files logger-ref programs-ref shaders-ref loading-system render-tasks-ref glslv glslf prog-name )
	true)
      false)))

(defn append-to-ref-list [render-tasks-ref lmbda]
  (dosync (ref-set render-tasks-ref (conj @render-tasks-ref lmbda))))

(defn rcgl-delete-glsl-program[render-context-ref render-tasks-ref prog-name]
  (let [{ programs-ref :glsl-programs-ref shaders-ref :glsl-shaders-ref
	  logger-ref :logger-ref } @render-context-ref]
    (append-to-ref-list render-tasks-ref #(rcglg-delete-program-and-shaders logger-ref % programs-ref shaders-ref prog-name))))

(defn rcgl-set-glsl-uniforms[render-context gl var-pair-seq rcgl-glsl-program]
  (let [logger-ref (render-context :logger-ref)]
    (rcglg-set-prog-uniforms logger-ref gl var-pair-seq rcgl-glsl-program)))

(defn rcgl-associate-new-shader [render-context-ref prog-name old-shader-name new-shader-name]
  (let [{ programs-ref :glsl-programs-ref shaders-ref :glsl-shaders-ref } @render-context-ref
	existing (@programs-ref prog-name)]
    (when existing
      (let [keyword (if (= ((existing :vert-shader) :filename) old-shader-name)
		      :vert-shader
		      (when (= ((existing :frag-shader) :filename) old-shader-name)
			:frag-shader))]
	(when keyword
	  (dosync (alter programs-ref (fn [programs] (assoc programs prog-name 
							    (assoc existing keyword
								   (assoc (existing keyword) :filename new-shader-name)))))))))))

(defn rcgl-load-shader [render-context-ref render-tasks-ref filename]
  (let [{ programs-ref :glsl-programs-ref shaders-ref :glsl-shaders-ref
	  loading-system :loading-system
	  logger-ref :logger-ref} @render-context-ref]
    (rcglg-begin-shader-load logger-ref programs-ref shaders-ref loading-system render-tasks-ref filename)))

;vbo type must be either :data or :index
;generator is a function that returns a sequence of numbers.  If they are float
;then you get a float buffer.  If they are bytes, then you get a byte buffer.
;Finally, if they are short, then you get a short buffer.  So pay attention
;when you are creating the sequence.
(defn rcgl-create-vbo [render-context-ref render-tasks-ref buf-name vbo-type generator]
  (let [{ vbos-ref :vbos-ref   
	  logger-ref :logger-ref } @render-context-ref]
    (append-to-ref-list render-tasks-ref #(rcglv-create-named-vbo logger-ref (. % getGL) vbos-ref buf-name vbo-type generator))))

(defn rcgl-delete-vbo [render-context-ref render-tasks-ref buf-name]
  (let [{ vbos-ref :vbos-ref
	  logger-ref :logger-ref } @render-context-ref]
    (append-to-ref-list render-tasks-ref #(rcglv-delete-named-vbo logger-ref (. % getGL) vbos-ref buf-name ))))

;Functions below are query functions of the render context.
;They take a non-ref'd context as they don't change the context
(defn rcgl-get-vbo [render-context name]
  (let [{ vbos-ref :vbos-ref } render-context
	retval (@vbos-ref name)]
    (if (rcglv-vbo-valid retval)
      retval
      nil)))

;Returns the glsl program mapped to this name
;or nil if the program doesn't exist or is invalid.
(defn rcgl-get-glsl-program[render-context prog-name]
  (let [{ programs-ref :glsl-programs-ref } render-context
	program (@programs-ref prog-name)
	prog-valid (rcglg-program-valid program)]
    (if prog-valid
      program
      nil)))


;unlike vbos or gl programs, the system is completely capable of
;creating surfaces during the render process.  Plus, there is never a good reason
;to pass processing off to another thread; you will always just block at the card
;trying to create them.
(defn rcgl-create-context-surface[render-context-ref render-tasks-ref sspec name]
  (append-to-ref-list render-tasks-ref 
		      #(rcglf-create-named-context-surface 
			(@render-context-ref :logger-ref) 
			(. % getGL) 
			(@render-context-ref :surfaces-ref) 
			sspec 
			name)))

;Create a context surface using a list of fallbacks
(defn rcgl-create-context-surface-seq[render-context-ref render-tasks-ref sspec-seq name]
  (append-to-ref-list render-tasks-ref 
		      #(rcglf-create-named-context-surface-seq 
			(@render-context-ref :logger-ref) 
			(. % getGL) 
			(@render-context-ref :surfaces-ref) 
			sspec-seq 
			name)))


;only runs if the surface exists already
(defn rcgl-update-context-surface[render-context-ref render-tasks-ref name width height]
  (append-to-ref-list render-tasks-ref 
		      #(rcglf-update-named-context-surface 
			(@render-context-ref :logger-ref) 
			(. % getGL) 
			(@render-context-ref :surfaces-ref) 
			name 
			width 
			height)))

(defn rcgl-delete-context-surface[render-context-ref render-tasks-ref name]
  (append-to-ref-list render-tasks-ref 
		      #(rcglf-delete-named-context-surface 
			(@render-context-ref :logger-ref) 
			(. % getGL) 
			(@render-context-ref :surfaces-ref) 
			name)))

(defn rcgl-get-context-surface[render-context name]
  (let [surfaces-ref (render-context :surfaces-ref)
	retval (@surfaces-ref name)]
    (if (rcglf-context-surface-valid-for-render retval)
      retval
      nil)))


(defn rcgl-allocate-context-texture[render-context-ref render-tasks-ref texture-spec name]
  (let [texture-map-ref (@render-context-ref :texture-map-ref)]
    (append-to-ref-list render-tasks-ref
			#(rcglt-create-context-texture
			  (.getGL %)
			  texture-map-ref
			  texture-spec
			  name))))


(defn rcgl-destroy-context-texture[render-context-ref render-tasks-ref name]
  (let [texture-map-ref (@render-context-ref :texture-map-ref)]
    (append-to-ref-list render-tasks-ref
			#(rcglt-destroy-context-texture
			  (.getGL %)
			  texture-map-ref
			  name))))

(defn rcgl-load-image-file
  "Load an image file using the rcgli.  File is scheduled for load later"
  [render-context-ref render-tasks-ref fname name]
  (let [image-map-ref (@render-context-ref :image-map-ref)]
    (rcgli-load-image-file render-tasks-ref image-map-ref fname name)))

(defn rcgl-destroy-named-image
  "Get rid of an image resource"
  [render-context-ref render-tasks-ref name]
  (let [image-map-ref (@render-context-ref :image-map-ref)]
    (append-to-ref-list render-tasks-ref
			(fn [_]
			  (rcgli-destroy-named-image
			   image-map-ref name)))))

(defn rcgl-get-named-image
  "Get an image resource"
  [render-context name]
  (let [image-map-ref (render-context :image-map-ref)]
    (@image-map-ref name)))
	
;This is called when all of the resources were destroyed through nefarious means.
;The ones that can be regenerated will be.
;returns a new render context
(defn rcgl-resources-destroyed[drawable render-context]
  (let [{ programs-ref :glsl-programs-ref shaders-ref :glsl-shaders-ref
	 vbos-ref :vbos-ref
	 surfaces-ref :surfaces-ref 
	 texture-map-ref :texture-map-ref
	 logger-ref :logger-ref } render-context
	  gl (.getGL drawable)]
    (rcglg-resources-released-reload-all-programs logger-ref drawable programs-ref shaders-ref)
    (rcglv-vbo-resources-destroyed logger-ref gl vbos-ref)
    (rcglf-context-surfaces-destroyed logger-ref gl surfaces-ref)
    (rcglt-reload-resources gl texture-map-ref))
  render-context)