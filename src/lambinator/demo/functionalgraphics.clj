(ns lambinator.demo.functionalgraphics)

(defn sin [theta] (Math/sin (Math/toRadians theta)))
(defn cos [theta] (Math/cos (Math/toRadians theta)))

(defstruct float3x3 :m11 :m12 :m13 :m21 :m22 :m23 :m31 :m32 :m33)
(defn float3x3-scale [x y]
  (struct float3x3 x 0 0 0 y 0 0 0 1) )
(defn float3x3-translate [x y]
  (struct float3x3 1 0 0 0 1 0 x y 1) )
(defn float3x3-rot [r]
  (struct float3x3    (cos r)  (sin r) 0
                   (- (sin r)) (cos r) 0
                   0           0       1))

(defn float3x3-mult [l r]
  (struct float3x3
    (+ (* (:m11 l) (:m11 r))
       (* (:m12 l) (:m21 r))
       (* (:m13 l) (:m31 r)))
    (+ (* (:m11 l) (:m12 r))
       (* (:m12 l) (:m22 r))
       (* (:m13 l) (:m32 r)))
    (+ (* (:m11 l) (:m13 r))
       (* (:m12 l) (:m23 r))
       (* (:m13 l) (:m33 r)))

    (+ (* (:m21 l) (:m11 r))
       (* (:m22 l) (:m21 r))
       (* (:m23 l) (:m31 r)))
    (+ (* (:m21 l) (:m12 r))
       (* (:m22 l) (:m22 r))
       (* (:m23 l) (:m32 r)))
    (+ (* (:m21 l) (:m13 r))
       (* (:m22 l) (:m23 r))
       (* (:m23 l) (:m33 r)))

    (+ (* (:m31 l) (:m11 r))
       (* (:m32 l) (:m21 r))
       (* (:m33 l) (:m31 r)))
    (+ (* (:m31 l) (:m12 r))
       (* (:m32 l) (:m22 r))
       (* (:m33 l) (:m32 r)))
    (+ (* (:m31 l) (:m13 r))
       (* (:m32 l) (:m23 r))
       (* (:m33 l) (:m33 r)))))
  
(defstruct float2   :x  :y)
(defn vec-transform [v m]
  (struct float2 
    (+ (* (:x v) (:m11 m)) 
       (* (:y v) (:m21 m))
       (* 1      (:m31 m)))
    (+ (* (:x v) (:m12 m)) 
       (* (:y v) (:m22 m))
       (* 1      (:m32 m)))))
       
(defn float3x3-identity []
  (struct float3x3 1 0 0 0 1 0 0 0 1))

(defn lines-to-local-lines [width height [start end]] 
  (let [half-width  (/ width  2)
        half-height (/ height 2)]
    [ (struct float2 (/ (- (:x start) half-width ) width) 
                     (/ (- (:y start) half-height) height))
      (struct float2 (/ (- (:x end  ) half-width ) width) 
                     (/ (- (:y end  ) half-height) height)) ] ) )

(defn transform-line [[start end] transform]
  [ (vec-transform start transform) (vec-transform end transform) ] )

(defn grid [width height lines]
  (let [local-lines (map lines-to-local-lines (repeat width) (repeat height) lines)]
    (fn [transform] (map transform-line local-lines (repeat transform)))))

(defn blank [] (fn [transform] []))

(defn beside [p1 p2]
  (fn [transform]
    (let [ left-transform  (float3x3-mult
                             (float3x3-mult (float3x3-translate -1/2 0)
                                            (float3x3-scale      1/2 1))
                             transform)
           right-transform (float3x3-mult
                             (float3x3-mult (float3x3-translate  1/2 0) 
                                            (float3x3-scale      1/2 1))
                             transform)
         ]
      (concat (p1 left-transform)
              (p2 right-transform)))))
            
(defn above [p1 p2]
  (fn [transform]
    (let [ above-transform (float3x3-mult
                             (float3x3-mult (float3x3-translate 0 -1/2)
                                            (float3x3-scale     1  1/2))
                             transform)
           below-transform (float3x3-mult  
                             (float3x3-mult (float3x3-translate 0  1/2)
                                            (float3x3-scale     1  1/2))
                             transform)
         ]
      (concat (p1 above-transform)
              (p2 below-transform)))))

(defn rot [p]
  (fn [transform]
    (p (float3x3-mult (float3x3-rot 90) transform))))

(defn quartet [p1 p2 p3 p4]
  (above (beside p3 p4) (beside p1 p2)))

(defn p-cycle [p]
  (quartet p (rot (rot (rot p))) (rot p) (rot (rot p))))

(def single-line
  (grid 2 2
    [ [ (struct float2 0 0) (struct float2 1 1)  ] ] ))

(def f-lines
  (grid 16 16
    [
      [ (struct float2 1 1 ) (struct float2 1  15) ]
      [ (struct float2 1 15) (struct float2 15 15) ]
      [ (struct float2 1 9 ) (struct float2 13 9 ) ]
    ]))

(defn instruction-from-pair [[start end]]
  (let [ w 1000
         h -1000
         half-w (/ w 2)
         half-h (/ h 2) 
         x1 (float (:x start)) 
         y1 (float (:y start))
         x2 (float (:x end  ))
         y2 (float (:y end  ))]
    (apply str 
      [ "M " (+ (* x1 w) half-w) " " (- (* y1 h) half-h) " "
        "L " (+ (* x2 w) half-w) " " (- (* y2 h) half-h) " " ])))

(defn svg-path-instructions [lines] 
  (apply str (map instruction-from-pair lines)))

(defn write-svg [name lines]
  (let [doc [ "<svg xmlns='http://www.w3.org/2000/svg'>"
              "<path d='"
              (svg-path-instructions lines)
              "' fill='none' stroke='black' stroke-width='1' />"
              "</svg>" ]
            ]
    (doto (new java.io.FileWriter name) (.write (apply str doc)) (.close))))