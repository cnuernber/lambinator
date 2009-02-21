(ns lambinator.rcgl.util
  (:use lambinator.util)
  (:import (javax.media.opengl GL)))

(defn rcglu-get-opengl-constant-name 
  "Given one of the ubiquitous opengl integers, find out what
its name is by looking up its member on the static gl class 
description"
  [constant]
  (let [field (first (util-find-static-fields-by-value "javax.media.opengl.GL" constant))]
    (if field
      (. field getName)
      "")))

(defn rcglu-get-gl-error
  "Return the current gl error."
  [gl]
  (. gl glGetError))

(defn rcglu-allocate-gl-item-fn
  "Allocate the given item.  A wrapper for a gl call
that takes a signature like glProgramiv.
The passed in function must take three arguments,
count, array, and offset"
  [lambda]
  (let [args (make-array Integer/TYPE 1)]
    (lambda 1 args 0)
    (aget args 0)))

(defmacro rcglu-allocate-gl-item 
  "A lot of gl calls have the form of
fn( int number, array ret-data, int offset )
this wraps creating the retvals and such"
  [gl fn-name]
  `(rcglu-allocate-gl-item-fn (fn [count# array# offset#] (. ~gl ~fn-name count# array# offset# ))))


(defmacro rcglu-release-gl-item
  "Release an opengl handle.  Just like allocate, a lot of
these functions have identical signatures just different names"
  [gl fn-name handle]
  `(let [args# (make-array Integer/TYPE 1)]
    (aset-int args# 0 ~handle)
    (.~gl ~fn-name 1 args# 0)))

(defmulti #^{:doc "Get the internal format for a specific combination of rc
format and datatype"}
  rcglu-gl-internal-format-from-rc-format-and-type #(vector %1 %2))
(defmethod rcglu-gl-internal-format-from-rc-format-and-type :default [arg1 arg2] GL/GL_RGBA8 )

(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:rgba :ubyte] [arg1 arg2] GL/GL_RGBA8 )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:rgba :ushort] [arg1 arg2] GL/GL_RGBA16 )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:rgba :half-float] [arg1 arg2] GL/GL_FLOAT_RGBA16_NV )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:rgba :float] [arg1 arg2] GL/GL_FLOAT_RGBA32_NV )

(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:rgb :ubyte] [arg1 arg2] GL/GL_RGB8 )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:rgb :ushort] [arg1 arg2] GL/GL_RGB16 )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:rgb :half-float] [arg1 arg2] GL/GL_FLOAT_RGB16_NV )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:rgb :float] [arg1 arg2] GL/GL_FLOAT_RGB32_NV )

(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:alpha :ubyte] [arg1 arg2] GL/GL_ALPHA8 )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:alpha :ushort] [arg1 arg2] GL/GL_ALPHA16 )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:alpha :half-float] [arg1 arg2] GL/GL_ALPHA16F_ARB )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:alpha :float] [arg1 arg2] GL/GL_ALPHA32F_ARB )

(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:lum-alpha :ubyte] [arg1 arg2] GL/GL_LUMINANCE_ALPHA8UI_EXT )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:lum-alpha :ushort] [arg1 arg2] GL/GL_LUMINANCE_ALPHA16UI_EXT  )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:lum-alpha :half-float] [arg1 arg2] GL/GL_LUMINANCE_ALPHA16F_ARB )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:lum-alpha :float] [arg1 arg2] GL/GL_LUMINANCE_ALPHA32F_ARB  )

(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:lum :ubyte] [arg1 arg2] GL/GL_LUMINANCE8 )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:lum :ushort] [arg1 arg2] GL/GL_LUMINANCE8UI_EXT )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:lum :half-float] [arg1 arg2] GL/GL_LUMINANCE16F_ARB )
(defmethod rcglu-gl-internal-format-from-rc-format-and-type [:lum :float] [arg1 arg2] GL/GL_LUMINANCE32F_ARB )

(defmulti #^{:doc "Return the gl format from an rc format"}
  rcglu-gl-format-from-rc-format identity)
(defmethod rcglu-gl-format-from-rc-format :default [arg] GL/GL_RGBA )
(defmethod rcglu-gl-format-from-rc-format :rgb [arg] GL/GL_RGB )
(defmethod rcglu-gl-format-from-rc-format :lum [arg] GL/GL_LUMINANCE )
(defmethod rcglu-gl-format-from-rc-format :lum-alpha [arg] GL/GL_LUMINANCE_ALPHA )
(defmethod rcglu-gl-format-from-rc-format :alpha [arg] GL/GL_ALPHA )

(defmulti #^{:doc "Return the gl datatype from an rc datatype" } 
  rcglu-gl-datatype-from-rc-datatype  identity)
(defmethod rcglu-gl-datatype-from-rc-datatype :default [arg] GL/GL_UNSIGNED_BYTE )
(defmethod rcglu-gl-datatype-from-rc-datatype :ushort [arg] GL/GL_UNSIGNED_SHORT )
(defmethod rcglu-gl-datatype-from-rc-datatype :half-float [arg] GL/GL_HALF_FLOAT_ARB )
(defmethod rcglu-gl-datatype-from-rc-datatype :float [arg] GL/GL_FLOAT )

(defmulti #^{:doc "Return the gl depth constant for a given rc depth constant"}
  rcglu-convert-depth-bits-to-gl-constant
  identity)
(defmethod rcglu-convert-depth-bits-to-gl-constant :default [arg] GL/GL_DEPTH_COMPONENT24)
(defmethod rcglu-convert-depth-bits-to-gl-constant :16      [arg] GL/GL_DEPTH_COMPONENT16)
(defmethod rcglu-convert-depth-bits-to-gl-constant :32      [arg] GL/GL_DEPTH_COMPONENT32)


(defmulti  #^{:doc "Return the opengl dataypte from a JVM datatype"}
  rcglu-gl-datatype-from-clojure-type 
  #(class %))
(defmethod rcglu-gl-datatype-from-clojure-type :default [_] GL/GL_FLOAT)
(defmethod rcglu-gl-datatype-from-clojure-type Short/TYPE [_] GL/GL_SHORT)
(defmethod rcglu-gl-datatype-from-clojure-type Byte/TYPE [_] GL/GL_BYTE)
(defmethod rcglu-gl-datatype-from-clojure-type Integer/TYPE [_] GL/GL_INT)

(defmulti #^{:doc "Return the opengl datatype size in bytes"}
  rcglu-gl-item-byte-size identity )
(defmethod rcglu-gl-item-byte-size :default [_] 4)
(defmethod rcglu-gl-item-byte-size GL/GL_SHORT [_] 2)
(defmethod rcglu-gl-item-byte-size GL/GL_BYTE [_] 1)
