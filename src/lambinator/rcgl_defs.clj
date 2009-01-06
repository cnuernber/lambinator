(in-ns 'lambinator.rcgl)
;Simple render context implementation that binds the
;render commands to sets of opengl calls


;The caches, fbo, texture, and shaders have reusable entries to some extent.
;this wraps the concept of finding the best entry if it exists else creating
;a new item.
(defn find-best-match-or-create-new [best-fn best-valid-fn new-fn recreate-fn]
  (let [best-result (best-fn)]
    (if (best-valid-fn best-result)
      (recreate-fn best-result)
      (new-fn best-result))))

(defn sspec-from-context-surface-index [all-surfaces idx]
  ((all-surfaces idx) :surface-spec))

(defn get-opengl-constant-name [constant]
  (let [field (first (find-static-fields-by-value "javax.media.opengl.GL" constant))]
    (if field
      (. field getName)
      "")))

(defn get-gl-error[gl]
  (. gl glGetError))

;A lot of gl calls have the form of
;fn( int number, array ret-data, int offset )
;this wraps creating the retvals and such
(defn allocate-gl-item[lmbda]
  (let [args (make-array Integer/TYPE 1)]
    (lmbda 1 args 0)
    (aget args 0)))

(defn release-gl-item[lmbda handle]
  (let [args (make-array Integer/TYPE 1)]
    (aset-int args 0 handle)
    (lmbda 1 args 0)))


(defmulti gl-internal-format-from-texture-format-and-type #(vector %1 %2))
(defmethod gl-internal-format-from-texture-format-and-type :default [arg1 arg2] GL/GL_RGBA8 )

(defmethod gl-internal-format-from-texture-format-and-type [:rgba :ubyte] [arg1 arg2] GL/GL_RGBA8 )
(defmethod gl-internal-format-from-texture-format-and-type [:rgba :ushort] [arg1 arg2] GL/GL_RGBA16 )
(defmethod gl-internal-format-from-texture-format-and-type [:rgba :half-float] [arg1 arg2] GL/GL_FLOAT_RGBA16_NV )
(defmethod gl-internal-format-from-texture-format-and-type [:rgba :float] [arg1 arg2] GL/GL_FLOAT_RGBA32_NV )

(defmethod gl-internal-format-from-texture-format-and-type [:rgb :ubyte] [arg1 arg2] GL/GL_RGB8 )
(defmethod gl-internal-format-from-texture-format-and-type [:rgb :ushort] [arg1 arg2] GL/GL_RGB16 )
(defmethod gl-internal-format-from-texture-format-and-type [:rgb :half-float] [arg1 arg2] GL/GL_FLOAT_RGB16_NV )
(defmethod gl-internal-format-from-texture-format-and-type [:rgb :float] [arg1 arg2] GL/GL_FLOAT_RGB32_NV )

(defmethod gl-internal-format-from-texture-format-and-type [:alpha :ubyte] [arg1 arg2] GL/GL_ALPHA8 )
(defmethod gl-internal-format-from-texture-format-and-type [:alpha :ushort] [arg1 arg2] GL/GL_ALPHA16 )
(defmethod gl-internal-format-from-texture-format-and-type [:alpha :half-float] [arg1 arg2] GL/GL_ALPHA16F_ARB )
(defmethod gl-internal-format-from-texture-format-and-type [:alpha :float] [arg1 arg2] GL/GL_ALPHA32F_ARB )

(defmethod gl-internal-format-from-texture-format-and-type [:lum-alpha :ubyte] [arg1 arg2] GL/GL_LUMINANCE_ALPHA8UI_EXT )
(defmethod gl-internal-format-from-texture-format-and-type [:lum-alpha :ushort] [arg1 arg2] GL/GL_LUMINANCE_ALPHA16UI_EXT  )
(defmethod gl-internal-format-from-texture-format-and-type [:lum-alpha :half-float] [arg1 arg2] GL/GL_LUMINANCE_ALPHA16F_ARB )
(defmethod gl-internal-format-from-texture-format-and-type [:lum-alpha :float] [arg1 arg2] GL/GL_LUMINANCE_ALPHA32F_ARB  )

(defmethod gl-internal-format-from-texture-format-and-type [:lum :ubyte] [arg1 arg2] GL/GL_LUMINANCE8 )
(defmethod gl-internal-format-from-texture-format-and-type [:lum :ushort] [arg1 arg2] GL/GL_LUMINANCE8UI_EXT )
(defmethod gl-internal-format-from-texture-format-and-type [:lum :half-float] [arg1 arg2] GL/GL_LUMINANCE16F_ARB )
(defmethod gl-internal-format-from-texture-format-and-type [:lum :float] [arg1 arg2] GL/GL_LUMINANCE32F_ARB )

(defn gl-internal-format-from-texture-spec [spec]
  (gl-internal-format-from-texture-format-and-type
   (spec :format)
   (spec :datatype)))

(defmulti gl-external-format-from-texture-format identity)
(defmethod gl-external-format-from-texture-format :rgba [arg] GL/GL_RGBA )
(defmethod gl-external-format-from-texture-format :rgb [arg] GL/GL_RGB )
(defmethod gl-external-format-from-texture-format :lum [arg] GL/GL_LUMINANCE )
(defmethod gl-external-format-from-texture-format :lum-alpha [arg] GL/GL_LUMINANCE_ALPHA )
(defmethod gl-external-format-from-texture-format :alpha [arg] GL/GL_ALPHA )

(defn gl-external-format-from-texture-spec [spec]
  (gl-external-format-from-texture-format (spec :format)))

(defmulti gl-external-datatype-from-texture-datatype identity)
(defmethod gl-external-datatype-from-texture-datatype :default [arg] GL/GL_UNSIGNED_BYTE )
(defmethod gl-external-datatype-from-texture-datatype :ushort [arg] GL/GL_UNSIGNED_SHORT )
(defmethod gl-external-datatype-from-texture-datatype :half-float [arg] GL/GL_HALF_FLOAT_ARB )
(defmethod gl-external-datatype-from-texture-datatype :float [arg] GL/GL_FLOAT )

(defn gl-external-datatype-from-texture-spec [spec]
  (gl-external-datatype-from-texture-datatype (spec :datatype)))

(defmulti convert-depth-bits-to-gl-constant identity)
(defmethod convert-depth-bits-to-gl-constant :default [arg] GL/GL_DEPTH_COMPONENT24)
(defmethod convert-depth-bits-to-gl-constant :16      [arg] GL/GL_DEPTH_COMPONENT16)
(defmethod convert-depth-bits-to-gl-constant :32      [arg] GL/GL_DEPTH_COMPONENT32)

(defmulti perform-render-command :render-command-type)

(defmethod perform-render-command :default [cmd context]
  (println "unrecognized render command")
  context)
(defmethod perform-render-command :scene-render-command [cmd context]
  (println "rendering the scene")
  context)