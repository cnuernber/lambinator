(ns lambinator.rcgl.image
  (:use (lambinator util fs)
	(lambinator.rcgl util))
  (:import (javax.media.opengl GL)
	   (com.sun.opengl.util.texture TextureIO)
	   (javax.imageio ImageIO)))

;The general idea behind images is that they are the result of image buffers.
;You create an image by specifying either a BufferedImage and a function to call
;when the gl resources need to be reloaded.  The function may return
;a buffered image or it may simply kick of an expensive process in another
;thread that then calls the normal create method with the same name.

(defstruct rcgl-image
  :buffered-image-func ;Function that may return a BufferedImage
  :gl-image          ;the IOTexture
  :gl-handle         ;the gl handle of the texture.  Thus this matches other apis
  :name              ;name of the image
)

(defn rcgli-valid-image
  "Is this a valid image"
  [rcgl-image]
  (and rcgl-image
       (rcgl-image :gl-image)
       (> (rcgl-image :gl-handle) 0)))

;synchronously create a new rcgl image
(defn rcgli-create-image
  "Create a new image"
  [buffered-image buffered-image-func name]
  (let [new-texture (TextureIO/newTexture buffered-image false)]
    (struct-map rcgl-image
      :buffered-image-func buffered-image-func
      :gl-image new-texture
      :gl-handle (.getTextureObject new-texture)
      :name name)))

;synchronously destroy an rcgl image
(defn rcgli-destroy-image
  "Destroy a given image"
  [rcgl-image]
  (when (rcgli-valid-image rcgl-image)
    (.dispose (rcgl-image :gl-image)))
  (assoc rcgl-image
    :gl-image nil
    :gl-handle 0))

(defn- remove-image
  "Remove an entry and return the old value"
  [image-map-ref name]
  (dosync
   (let [existing (@image-map-ref name)]
     (alter image-map-ref dissoc name)
     existing)))

(defn rcgli-destroy-named-image
  "Destroy a named image"
  [image-map-ref name]
  (let [existing (remove-image image-map-ref name)]
    (when existing
      (rcgli-destroy-image existing))))

(defn rcgli-create-named-image
  "Create a named entry"
  [image-map-ref buffered-image buffered-image-func name]
  (rcgli-destroy-named-image image-map-ref name)
  (let [new-image (rcgli-create-image buffered-image buffered-image-func name)]
    (dosync
     (alter image-map-ref assoc name new-image))))

(defn rcgli-resources-destroyed[image-map-ref]
  (let [new-images (map (fn [[name image]] ;Generate list of new images
			  (let [func (image :buffered-image-func)
				new-buf (when func
					  (func))
				new-image (when new-buf
					    (rcgli-create-image
					     new-buf 
					     func 
					     (image :name)))]
			    [name new-image]))
			@image-map-ref)
	new-images (filter (fn [[name image]] ;remove nil entries
			     (not (nil? image)))
			   new-images)
	new-images (mapcat identity new-images)] ;create new single list
    (util-update-map-ref image-map-ref new-images)))

(defn rcgli-file-or-resource-to-buffered-image
  "Reads the given resource creating a buffered image.  Does a synchronous load.
Will work for at least png and jpg."
  [fname]
  (let [input-stream (fs-get-item-input-stream fname)]
    (when input-stream
      (ImageIO/read input-stream))))

(declare rcgli-load-image-file)

(defn- load-image 
  "Meant to be done in an io thread, load the image into a buffered
image and fire off a render task to finish the loading and upload information
to opengl"
  [load-agent render-task-ref image-map-ref fname name]
  (try
   (let [new-image (rcgli-file-or-resource-to-buffered-image fname)]
     (when new-image
       ;reload images in a thread, not in the render thread as that can take
       ;a really long time.
       (let [reload-fn (fn []
			 (rcgli-load-image-file render-task-ref image-map-ref fname name)
			 nil)]
       (dosync 
	(alter render-task-ref conj (fn [drawable]
				      (rcgli-create-named-image 
				       image-map-ref
				       new-image
				       reload-fn
				       name)))))))
   (catch Exception e
     (.printStackTrace e))))

(defn rcgli-load-image-file 
  "Load an image file on a background thread using the loading system.
This assumes the loading process will be io bound."
  [render-task-ref image-map-ref fname name]
  (let [load-agent (agent {})]
    (send-off load-agent load-image render-task-ref image-map-ref fname name)))