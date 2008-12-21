(in-ns 'lambinator.rc)

(defn create_texture_spec [format type width height]
  (struct texture_spec type format (list width height)))

(defn create_mipmapped_texture_spec [texture count]
     (struct mipmapped_texture_spec texture count))

(defn create_color [r g b a]
  (struct color r g b a))

(defn create_surface_spec [depth_bits stencil texture_spec clear_color]
  (struct surface_spec depth_bits stencil texture_spec clear_color))

(defn create_scene_render_command [surface_spec clear_color]
  (struct scene_render_command :scene_render_command surface_spec clear_color))


;do details; type and format match
(defn texture_details_match [texture1 texture2]
  (and (== (texture1 :texture_type)
	   (texture2 :texture_type))
       (== (texture1 :format)
	   (texture2 :format))))

;ignoring width and height, could you substitute
;surface1 for surface2
(defn surface_details_match [surface1 surface2]
  (and (texture_details_match (surface1 :texture_spec)
			      (surface2 :texture_spec))
       (== (surface1 :depth_bits)
	   (surface2 :depth_bits))
       (== (surface1 :stencil)
	   (surface2 :stencil))))

;how many bytes would you have to allocate to
;fit desired in surface
;Assume you aren't going to shrink a surface
(defn bytes_required [surface desired]
  (let [sw (first ((surface :texture_spec) :size))
	sh (second ((surface :texture_spec) :size))
	dw (first ((desired :texture_spec) :size))
	dh (second ((desired :texture_spec) :size))
	ssize (* sw sh)
	max (fn [s d] (if (> d s) d s))]
    (if (and (>= sw dw)
	     (>= sh dh))
      0
      (* (max sw dw) (max sh dh)))))

(defn overdraw [surface desired]
  (let [sw (first ((surface :texture_spec) :size))
	sh (second ((surface :texture_spec) :size))
	dw (first ((desired :texture_spec) :size))
	dh (second ((desired :texture_spec) :size))]
    (* sw sh)))
	    