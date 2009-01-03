(in-ns 'lambinator.rcgl)
;Simple render context implementation that binds the
;render commands to sets of opengl calls


;The caches, fbo, texture, and shaders have reusable entries to some extent.
;this wraps the concept of finding the best entry if it exists else creating
;a new item.
(defn find_best_match_or_create_new [best_fn best_valid_fn new_fn recreate_fn]
  (let [best_result (best_fn)]
    (if (best_valid_fn best_result)
      (recreate_fn best_result)
      (new_fn best_result))))

(defn sspec_from_context_surface_index [all_surfaces idx]
  ((all_surfaces idx) :surface_spec))

(defn get_opengl_constant_name [constant]
  (let [field (first (find_static_fields_by_value "javax.media.opengl.GL" constant))]
    (if field
      (. field getName)
      "")))

(defn get_gl_error[gl]
  (. gl glGetError))

;A lot of gl calls have the form of
;fn( int number, array ret_data, int offset )
;this wraps creating the retvals and such
(defn allocate_gl_item[lmbda]
  (let [args (make-array Integer/TYPE 1)]
    (lmbda 1 args 0)
    (aget args 0)))

(defn release_gl_item[lmbda handle]
  (let [args (make-array Integer/TYPE 1)]
    (aset-int args 0 handle)
    (lmbda 1 args 0)))


(defmulti gl_internal_format_from_texture_format_and_type #(vector %1 %2))
(defmethod gl_internal_format_from_texture_format_and_type :default [arg1 arg2] GL/GL_RGBA8 )

(defmethod gl_internal_format_from_texture_format_and_type [:rgba :ubyte] [arg1 arg2] GL/GL_RGBA8 )
(defmethod gl_internal_format_from_texture_format_and_type [:rgba :ushort] [arg1 arg2] GL/GL_RGBA16 )
(defmethod gl_internal_format_from_texture_format_and_type [:rgba :half_float] [arg1 arg2] GL/GL_FLOAT_RGBA16_NV )
(defmethod gl_internal_format_from_texture_format_and_type [:rgba :float] [arg1 arg2] GL/GL_FLOAT_RGBA32_NV )

(defmethod gl_internal_format_from_texture_format_and_type [:rgb :ubyte] [arg1 arg2] GL/GL_RGB8 )
(defmethod gl_internal_format_from_texture_format_and_type [:rgb :ushort] [arg1 arg2] GL/GL_RGB16 )
(defmethod gl_internal_format_from_texture_format_and_type [:rgb :half_float] [arg1 arg2] GL/GL_FLOAT_RGB16_NV )
(defmethod gl_internal_format_from_texture_format_and_type [:rgb :float] [arg1 arg2] GL/GL_FLOAT_RGB32_NV )

(defmethod gl_internal_format_from_texture_format_and_type [:alpha :ubyte] [arg1 arg2] GL/GL_ALPHA8 )
(defmethod gl_internal_format_from_texture_format_and_type [:alpha :ushort] [arg1 arg2] GL/GL_ALPHA16 )
(defmethod gl_internal_format_from_texture_format_and_type [:alpha :half_float] [arg1 arg2] GL/GL_ALPHA16F_ARB )
(defmethod gl_internal_format_from_texture_format_and_type [:alpha :float] [arg1 arg2] GL/GL_ALPHA32F_ARB )

(defmethod gl_internal_format_from_texture_format_and_type [:lum_alpha :ubyte] [arg1 arg2] GL/GL_LUMINANCE_ALPHA8UI_EXT )
(defmethod gl_internal_format_from_texture_format_and_type [:lum_alpha :ushort] [arg1 arg2] GL/GL_LUMINANCE_ALPHA16UI_EXT  )
(defmethod gl_internal_format_from_texture_format_and_type [:lum_alpha :half_float] [arg1 arg2] GL/GL_LUMINANCE_ALPHA16F_ARB )
(defmethod gl_internal_format_from_texture_format_and_type [:lum_alpha :float] [arg1 arg2] GL/GL_LUMINANCE_ALPHA32F_ARB  )

(defmethod gl_internal_format_from_texture_format_and_type [:lum :ubyte] [arg1 arg2] GL/GL_LUMINANCE8 )
(defmethod gl_internal_format_from_texture_format_and_type [:lum :ushort] [arg1 arg2] GL/GL_LUMINANCE8UI_EXT )
(defmethod gl_internal_format_from_texture_format_and_type [:lum :half_float] [arg1 arg2] GL/GL_LUMINANCE16F_ARB )
(defmethod gl_internal_format_from_texture_format_and_type [:lum :float] [arg1 arg2] GL/GL_LUMINANCE32F_ARB )

(defn gl_internal_format_from_texture_spec [spec]
  (gl_internal_format_from_texture_format_and_type
   (spec :format)
   (spec :datatype)))

(defmulti gl_external_format_from_texture_format identity)
(defmethod gl_external_format_from_texture_format :rgba [arg] GL/GL_RGBA )
(defmethod gl_external_format_from_texture_format :rgb [arg] GL/GL_RGB )
(defmethod gl_external_format_from_texture_format :lum [arg] GL/GL_LUMINANCE )
(defmethod gl_external_format_from_texture_format :lum_alpha [arg] GL/GL_LUMINANCE_ALPHA )
(defmethod gl_external_format_from_texture_format :alpha [arg] GL/GL_ALPHA )

(defn gl_external_format_from_texture_spec [spec]
  (gl_external_format_from_texture_format (spec :format)))

(defmulti gl_external_datatype_from_texture_datatype identity)
(defmethod gl_external_datatype_from_texture_datatype :default [arg] GL/GL_UNSIGNED_BYTE )
(defmethod gl_external_datatype_from_texture_datatype :ushort [arg] GL/GL_UNSIGNED_SHORT )
(defmethod gl_external_datatype_from_texture_datatype :half_float [arg] GL/GL_HALF_FLOAT_ARB )
(defmethod gl_external_datatype_from_texture_datatype :float [arg] GL/GL_FLOAT )

(defn gl_external_datatype_from_texture_spec [spec]
  (gl_external_datatype_from_texture_datatype (spec :datatype)))

(defmulti convert_depth_bits_to_gl_constant identity)
(defmethod convert_depth_bits_to_gl_constant :default [arg] GL/GL_DEPTH_COMPONENT24)
(defmethod convert_depth_bits_to_gl_constant :16      [arg] GL/GL_DEPTH_COMPONENT16)
(defmethod convert_depth_bits_to_gl_constant :32      [arg] GL/GL_DEPTH_COMPONENT32)

(defmulti perform_render_command :render_command_type)

(defmethod perform_render_command :default [cmd context]
  (println "unrecognized render command")
  context)
(defmethod perform_render_command :scene_render_command [cmd context]
  (println "rendering the scene")
  context)