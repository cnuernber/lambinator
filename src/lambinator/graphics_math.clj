(ns lambinator.graphics-math)

;A matrix44 is a vector.
;All the items are just vectors.  Makes life easy.
;quaternions, matrix44, matrix33, and, well, vectors.

(defn- gm-make-float-vec [vec]
  (apply vector (map float vec)))

(defmacro gm-identity[rows columns]
  `(gm-make-float-vec [~@(mapcat (fn [row#]
				   (map (fn [column#]
					  (if (== row# column#)
					    1
					    0))
					(range columns)))
				 (range rows))]))
				   
(defonce gm-identity-44
  (gm-identity 4 4))

(defonce gm-identity-33
  (gm-identity 3 3))


(defmacro gm-vdot[v1 v2 len]
  `(+ ~@(map (fn [idx#]
	       `(* (~v1 ~idx#) (~v2 ~idx#)))
	     (range len))))

(defn gm-vdot-4[v1 v2]
  (gm-vdot v1 v2 4))
	       
(defn gm-vdot-3[v1 v2]
  (gm-vdot v1 v2 3))

(defmacro gm-matrix-address[row column rows columns]
  `(+ (* ~row ~columns) ~column))

(defmacro gm-matrix-value [matrix row column rows columns]
  `(~matrix (gm-matrix-address ~row ~column ~rows ~columns)))

(defmacro gm-mcol
  "Get the column of a given matrix"
  [matrix col rows columns]
  `[ ~@(map (fn [idx#]
	      `(gm-matrix-value ~matrix ~idx# ~col ~rows ~columns))
	      (range rows))])

(defmacro gm-mrow-op
  "Get the row of a matrix, performing operation on it"
  [matrix row rows columns operation]
  `[ ~@(map (fn [idx#]
	      `(~operation (gm-matrix-value ~matrix ~row ~idx# ~rows ~columns)))
	    (range columns))])

(defmacro gm-mrow
  "Get the row of a given matrix"
  [matrix row rows columns]
  `(gm-mrow-op ~matrix ~row ~rows ~columns identity))


(defn gm-mcol-44 
  "Get a column from a 44 matrix"
  [matrix col]
  (gm-mcol matrix col 4 4))

(defn gm-mrow-44 
  "Get a row from a 44 matrix"
  [matrix row]
  (gm-mrow matrix row 4 4))

(defn gm-mcol-33 
  "Get a column from a 33 matrix"
  [matrix col]
  (gm-mcol matrix col 3 3))

(defn gm-mrow-33 
  "Get a row from a 44 matrix"
  [matrix row]
  (gm-mrow matrix row 3 3))

(defn gm-upper-33
  "Get the upper 3x3 from a 4x4 matrix"
  [m44]
  [(m44 0) (m44 1) (m44 2)
   (m44 4) (m44 5) (m44 6)
   (m44 8) (m44 9) (m44 10)])

(defn gm-33-44
  "Create a 44 matrix from a 33 matrix"
  [m33]
  (assoc gm-identity-44 
    0 (m33 0) 1 (m33 1) 2 (m33 2)
    4 (m33 3) 5 (m33 4) 6 (m33 5)
    8 (m33 6) 9 (m33 7) 10 (m33 8)))

(defmacro gm-mrows
  "Get each row of the matrix as a distinct vector"
  [matrix rows columns]
  `[~@(map (fn [idx#]
	     `(gm-mrow ~matrix ~idx# ~rows ~columns))
	   (range rows))])

(defmacro gm-mcols
  "Get each column of the matrix as a distinct vector"
  [matrix rows columns]
  `[~@(map (fn [idx#]
	     `(gm-mcol ~matrix ~idx# ~rows ~columns))
	   (range columns))])

(defmacro gm-printm
  "Print a matrix in a form that is somewhat readable"
  [matrix rows columns]
  `(doseq [row# (gm-mrows ~matrix ~rows ~columns)]
     (println row#)))

(defmacro gm-mm
  "Multiply one square matrix by another, assuming both have 
identical rows and column counts"
  [m1 m2 rows]
  `(let [~'mrows (gm-mrows ~m1 ~rows ~rows)
	 ~'mcolumns (gm-mcols ~m2 ~rows ~rows)]
     [~@(mapcat (fn [row#]
		  (map (fn [col#]
			 `(gm-vdot (~'mrows ~row#) (~'mcolumns ~col#) ~rows))
		       (range rows)))
		(range rows))]))

(defn gm-mm-44
  "Multiply two 4x4 matrixes"
  [m1 m2]
  (gm-mm m1 m2 4))

(defn gm-mm-33
  "Multiply two 3x3 matrixes"
  [m1 m2]
  (gm-mm m1 m2 3))

(defmacro gm-compare-rows[matrix r1 r2 column rows columns]
  `(let [v1# (gm-matrix-value ~matrix ~r1 ~column ~rows ~columns)
	 v2# (gm-matrix-value ~matrix ~r2 ~column ~rows ~columns)]
     (if (> v1# v2#)
       ~r1
       ~r2)))

(defmacro gm-find-max-row
  "starting from column and row and increasing,
return the max element of a given column and row"
  [matrix row column rows columns]
  `(loop [max-row# ~row
	  cur-row# (inc ~row)]
     (if (< (Math/abs cur-row#) (Math/abs ~rows))
       (recur (gm-compare-rows ~matrix max-row# cur-row# ~column ~rows ~columns)
	      (inc cur-row#))
       max-row#)))

(defmacro gm-swap-rows 
  "Swap the rows of a given matrix"
  [matrix r1 r2 rows columns]
  (let [vals-fn# (fn [row1# row2#]
		   (mapcat (fn [col#]
			     `[(gm-matrix-address ~row1# ~col# ~rows ~columns)
			       (gm-matrix-value ~matrix ~row2# ~col# ~rows ~columns)])
			   (range columns)))]
    `(assoc ~matrix ~@(concat (vals-fn# r2 r1) (vals-fn# r1 r2)))))

(defmacro gm-insert-row
  "Insert this vector as a row in the matrix"
  [matrix vect row rows columns]
  `(assoc ~matrix ~@(mapcat (fn [col#]
			      `[(gm-matrix-address ~row ~col# ~rows ~columns)
				(~vect ~col#)])
			    (range columns))))

(defmacro gm-pivot-matrixes
  "Find the max row at or below row and pivot such that it is now
at position row"
  [m1 m2 row column rows columns]
  `(let [max-row# (gm-find-max-row ~m1 ~row ~column ~rows ~columns)]
     (if (== max-row# ~row)
       [~m1 ~m2]
       [(gm-swap-rows ~m1 ~row max-row# ~rows ~columns)
	(gm-swap-rows ~m2 ~row max-row# ~rows ~columns)])))

(defmacro gm-subtract-row
  "Subtract a vector from a row"
  [m1 vect row rows columns])
  

(defmacro gm-multiply-matrixes-rows
  [m1 m2 mult row rows columns]
  `(let [new-op# (fn [arg#] (* ~mult arg#))
	 insert-fn# (fn [matrix#]
		      (gm-insert-row matrix# 
				     (gm-mrow-op matrix# ~row ~rows ~columns new-op#) 
				     ~row 
				     ~rows 
				     ~columns))]
     [(insert-fn# ~m1)
      (insert-fn# ~m2)]))

(defmacro gm-subtract-row
  "Subtract a multiple of r1 from r2, starting from
column"
  [m1 r1 r2 mult column rows columns]
  `(let [mult-fn# (fn [item#] (* item# ~mult))
	 ~'sub-row (gm-mrow-op ~m1 ~r1 ~rows ~columns mult-fn#)]
     (assoc ~m1 
       ~@(mapcat (fn [col#]
		   `[(gm-matrix-address ~r2 ~col# ~rows ~columns)
		     (- (gm-matrix-value ~m1 ~r2 ~col# ~rows ~columns)
			(~'sub-row ~col#))])
		 (range column columns)))))
	 
(defmacro gm-subtract-matrixes-rows
  "r2 - r1"
  [m1 m2 r1 r2 column rows columns]
  `(let [mult# (gm-matrix-value ~m1 ~r2 ~column ~rows ~columns)
	 res1# (gm-subtract-row ~m1 ~r1 ~r2 mult# ~r1 ~rows ~columns)
	 res2# (gm-subtract-row ~m2 ~r1 ~r2 mult# ~column ~rows ~columns)]
     [res1# res2#]))

(defmacro gm-gj-forward-elim[matrix result rows columns]
  (let [elim-row# (fn [row#]
		    `(let [[~'matrix ~'result] (gm-pivot-matrixes ~'matrix ~'result ~row# ~row# ~rows ~columns)
			   divisor# (gm-matrix-value ~'matrix ~row# ~row# ~rows ~columns)]
		       (when (> (Math/abs divisor#) 0)
			 (let [~'multiplier (/ 1.0 divisor#) ;zero out first item
			       [~'matrix ~'result] (gm-multiply-matrixes-rows ~'matrix ~'result ~'multiplier ~row# ~rows ~columns)
			       ~@(mapcat (fn [idx#]
					   `([~'matrix ~'result] 
					       (gm-subtract-matrixes-rows ~'matrix ~'result ~row# ~idx# ~row# ~rows ~columns)))
					 (range (inc row#) rows))]
			   [~'matrix ~'result]))))]
    `(let [~'matrix ~matrix
	   ~'result ~result
	   ;forward elmination
	   ~@(mapcat (fn [idx#]
		       `([~'matrix ~'result] ~(elim-row# idx#)))
		     (range  (dec rows)))
	   ;last row is a little different
	   ~'divisor (gm-matrix-value ~'matrix ~(dec rows) ~(dec rows) ~rows ~columns)]
       (when (> (Math/abs ~'divisor) 0)
	 (let [~'multiplier (/ 1.0 ~'divisor)
	       [~'matrix ~'result] (gm-multiply-matrixes-rows ~'matrix ~'result ~'multiplier ~(dec rows) ~rows ~columns)]
       [~'matrix ~'result])))))

(defmacro gm-gj-backprop-row
  "back-propogate the row upwards such that rows above have
a zero at column"
  [matrix result row rows columns]
  (let [back-elim# (fn [bot-row# top-row#]
		     `(let [val# (gm-matrix-value ~'matrix ~top-row# ~bot-row# ~rows ~columns)]
			[(gm-subtract-row ~'matrix ~bot-row# ~top-row# val# 0 ~rows ~columns)
			 (gm-subtract-row ~'result ~bot-row# ~top-row# val# 0 ~rows ~columns)]))]
    `(let [~'matrix ~matrix
	   ~'result ~result
	   ~@(mapcat (fn [top-row#]
		       `([~'matrix ~'result] ~(back-elim# row top-row#)))
		     (range (dec row) -1 -1))]
       [~'matrix ~'result])))

(defmacro gm-gj-backprop-rows
  "Back propagate rows to zero out items above"
  [m1 m2 rows columns]
  `(let [~'matrix ~m1
	 ~'result ~m2
	 ~@(mapcat (fn [bot-row#]
		     `([~'matrix ~'result] (gm-gj-backprop-row ~'matrix ~'result ~bot-row# ~rows ~columns)))
		   (range (dec rows) 0 -1))]
     [~'matrix ~'result]))

(defmacro gm-gj-invert
  "Invert a matrix throw gauss-jordan elimination"
  [m1 m2 rows columns]
  `(let [[matrix# result#] (gm-gj-forward-elim ~m1 ~m2 ~rows ~columns)]
     (when (and matrix# result#)
       (gm-gj-backprop-rows matrix# result# ~rows ~columns))))
 			   
(defn gm-gj-invert-44
  "Invert a 44 matrix through gauss-jordan elimination"
  [matrix]
  ((gm-gj-invert matrix gm-identity-44 4 4) 1))

(defn gm-gj-invert-33
  "Invert a 33 matrix through gauss-jordan elimination"
  [matrix]
  ((gm-gj-invert matrix gm-identity-33 3 3) 1))

(defmacro gm-transpose
  "Transpose a matrix"
  [matrix rows columns]
  `[~@(mapcat (fn [row#]
		(map (fn [col#]
		       `(gm-matrix-value ~matrix ~col# ~row# ~rows ~columns))
		     (range columns)))
	      (range rows))])

(defn gm-transpose-44
  "Transpose a 44 matrix"
  [matrix]
  (gm-transpose matrix 4 4))

(defn gm-transpose-33
  "Transpose a 33 matrix"
  [matrix]
  (gm-transpose matrix 3 3))

(defn gm-scale-44
  "scale a matrix"
  [sx sy sz]
  (assoc gm-identity-44 0 sx 5 sy 10 sz))

(defn gm-trans-44
  "translate a matrix"
  [tx ty tz]
  (assoc gm-identity-44 12 tx 13 ty 14 tz))


(defn gm-axis-angle-to-quat
  "Given an axis and an angle, return a quaternion"
  [axis angle]
  (let [half-angle (/ angle 2)
	sin-angle (Math/sin half-angle)]
    [(* (axis 0) sin-angle)
     (* (axis 1) sin-angle)
     (* (axis 2) sin-angle)
     (Math/cos half-angle)]))

(defn gm-quat-mult
  "Multiply two quaternions"
  [qL qR]
  (let [qLx (qL 0)
	qLy (qL 1)
	qLz (qL 2)
	qLw (qL 3)
	qRx (qR 0)
	qRy (qR 1)
	qRz (qR 2)
	qRw (qR 3)]
    [(- (+ (* qLw qRx) (* qLx qRw) (* qLy qRz)) (* qLz qRy))
     (- (+ (* qLw qRy) (* qLy qRw) (* qLz qRx)) (* qLx qRz))
     (- (+ (* qLw qRz) (* qLz qRw) (* qLx qRy)) (* qLy qRx))
     (- (* qLw qRw) (* qLx qRx) (* qLy qRy) (* qLz qRz))]))

(defn gm-quat-to-matrix
  "Given a quaternion, produce a 3x3 matrix"
  [quat]
  (let [x (quat 0)
	y (quat 1)
	z (quat 2)
	w (quat 3)
	sqx (* x x)
	sqy (* y y)
	sqz (* z z)
	sqw (* w w)
	invs (/ 1 (+ sqx sqy sqz sqw))
	m00 (* (+ sqw (- sqx sqy sqz)) invs)
	m11 (* (+ sqw (- sqy sqx sqz)) invs)
	m22 (* (+ sqw (- sqz sqx sqy)) invs)
	tmp1 (* x y)
	tmp2 (* z w)
	m10 (* 2 invs (+ tmp1 tmp2))
	m01 (* 2 invs (- tmp1 tmp2))
	tmp1 (* x z)
	tmp2 (* y w)
	m20 (* 2 invs (- tmp1 tmp2))
	m02 (* 2 invs (+ tmp1 tmp2))
	tmp1 (* y z)
	tmp2 (* x w)
	m21 (* 2 invs (+ tmp1 tmp2))
	m12 (* 2 invs (- tmp1 tmp2))]
    [m00 m01 m02
     m10 m11 m12
     m20 m21 m22]))