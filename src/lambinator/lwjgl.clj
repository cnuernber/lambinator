(ns lambinator.lwjgl
  (:import (org.lwjgl.opengl AWTGLCanvas EXTFramebufferObject GL11 GL12 GL14 Display)
	   (javax.swing JFrame SwingUtilities JLabel)
	   (java.nio ByteBuffer IntBuffer ByteOrder)))

(defn check-gl-error []
  (let [error (GL11/glGetError)]
    (when-not (== 0 error)
      (throw (Exception. (.toString error))))))

(defn direct-int-buffer [size]
  (let [byte-buffer (ByteBuffer/allocateDirect (* size 4))]
    (.order byte-buffer (ByteOrder/nativeOrder))
    (.asIntBuffer byte-buffer)))

(defn allocate-texture-fbo [width height]
  (println "Attempting to allocate fbos")
  (let [tex-buf (direct-int-buffer 1)
	fbo-buf (direct-int-buffer 1)]
    (GL11/glClearColor 0 0 0 0)
    (check-gl-error)
    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
    (check-gl-error)
    (GL11/glGenTextures tex-buf)
    (check-gl-error)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D (.get tex-buf 0))
    (check-gl-error)
    (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    ;(GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL12/GL_CLAMP_TO_EDGE)
    ;(GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL12/GL_CLAMP_TO_EDGE)

    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA8 width height 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE nil)    
    (check-gl-error)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D, 0)
    (check-gl-error)
    
    (EXTFramebufferObject/glGenFramebuffersEXT fbo-buf)
    (check-gl-error)
    (EXTFramebufferObject/glBindFramebufferEXT EXTFramebufferObject/GL_FRAMEBUFFER_EXT (.get fbo-buf 0))
    (check-gl-error)
    (EXTFramebufferObject/glFramebufferTexture2DEXT EXTFramebufferObject/GL_FRAMEBUFFER_EXT 
						    EXTFramebufferObject/GL_COLOR_ATTACHMENT0_EXT 
						    GL11/GL_TEXTURE_2D 
						    (.get tex-buf 0) 0)
    (check-gl-error)
    (let [status (EXTFramebufferObject/glCheckFramebufferStatusEXT EXTFramebufferObject/GL_FRAMEBUFFER_EXT)]
      (println status)
      (EXTFramebufferObject/glBindFramebufferEXT EXTFramebufferObject/GL_FRAMEBUFFER_EXT 0)
      (EXTFramebufferObject/glDeleteFramebuffersEXT fbo-buf)
      (GL11/glDeleteTextures tex-buf)
      status)))

(defn gl-render [canvas]
  (try
   (GL11/glGetError)
   (let [status (allocate-texture-fbo 256 256)]
     (.swapBuffers canvas))
   (catch Exception e (.printStackTrace e))))

(defn create-canvas-proxy []
  (proxy [AWTGLCanvas] []
    (paintGL [] (gl-render this))))
	     

(defn create-view []
  (let [canvas-ref (ref nil)]
    (SwingUtilities/invokeLater 
     (fn []
       (let [frame (JFrame.)
	     canvas (create-canvas-proxy)
	     test-label (JLabel. "TestLabel")
	     content-pane (.getContentPane frame)]
	 (.setSize frame 200 200)
	 (.add content-pane canvas)
	 (.setVisible canvas true)
	 (.show frame)
	 (dosync (ref-set canvas-ref canvas)))))
    canvas-ref))

(defn run-display
  []
  (Display/create)
  (Display/update)
  (loop [count 10]
    (Display/update)
    (allocate-texture-fbo 256 256)
    (when-not (== 0 count)
      (recur (dec count))))
  (Display/destroy))

(defn- -main [& args]
  (create-view))
      