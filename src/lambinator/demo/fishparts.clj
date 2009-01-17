(ns lambinator.demo.fishparts
  (:use lambinator.demo.functionalgraphics))


(def *p* 
  (grid 16 16
    [
      [ (struct float2 4 4) (struct float2 6 0) ]
      [ (struct float2 0 3) (struct float2 3 4) ]
      [ (struct float2 3 4) (struct float2 0 8) ]
      [ (struct float2 0 8) (struct float2 0 3) ]
      [ (struct float2 4 5) (struct float2 7 6) ]
      [ (struct float2 7 6) (struct float2 4 10) ]
      [ (struct float2 4 10) (struct float2 4 5) ]
      [ (struct float2 11 0) (struct float2 10 4) ]
      [ (struct float2 10 4) (struct float2 8 8) ]
      [ (struct float2 8 8) (struct float2 4 13) ]
      [ (struct float2 4 13) (struct float2 0 16) ]
      [ (struct float2 11 0) (struct float2 14 2) ]
      [ (struct float2 14 2) (struct float2 16 2) ]
      [ (struct float2 10 4) (struct float2 13 5) ]
      [ (struct float2 13 5) (struct float2 16 4) ]
      [ (struct float2 9 6) (struct float2 12 7) ]
      [ (struct float2 12 7) (struct float2 16 6) ]
      [ (struct float2 8 8) (struct float2 12 9) ]
      [ (struct float2 12 9) (struct float2 16 8) ]
      [ (struct float2 8 12) (struct float2 16 10) ]
      [ (struct float2 0 16) (struct float2 6 15) ]
      [ (struct float2 6 15) (struct float2 8 16) ]
      [ (struct float2 8 16) (struct float2 12 12) ]
      [ (struct float2 12 12) (struct float2 16 12) ]
      [ (struct float2 10 16) (struct float2 12 14) ]
      [ (struct float2 12 14) (struct float2 16 13) ]
      [ (struct float2 12 16) (struct float2 13 15) ]
      [ (struct float2 13 15) (struct float2 16 14) ]
      [ (struct float2 14 16) (struct float2 16 15) ]]))

(def *q*
  (grid 16 16
    [
      [ (struct float2 2 0) (struct float2 4 5) ]
      [ (struct float2 4 5) (struct float2 4 7) ]
      [ (struct float2 4 0) (struct float2 6 5) ]
      [ (struct float2 6 5) (struct float2 6 7) ]
      [ (struct float2 6 0) (struct float2 8 5) ]
      [ (struct float2 8 5) (struct float2 8 8) ]
      [ (struct float2 8 0) (struct float2 10 6) ]
      [ (struct float2 10 6) (struct float2 10 9) ]
      [ (struct float2 10 0) (struct float2 14 11) ]
      [ (struct float2 12 0) (struct float2 13 4) ]
      [ (struct float2 13 4) (struct float2 16 8) ]
      [ (struct float2 16 8) (struct float2 15 10) ]
      [ (struct float2 15 10) (struct float2 16 16) ]
      [ (struct float2 16 16) (struct float2 12 10) ]
      [ (struct float2 12 10) (struct float2 6 7) ]
      [ (struct float2 6 7) (struct float2 4 7) ]
      [ (struct float2 4 7) (struct float2 0 8) ]
      [ (struct float2 13 0) (struct float2 16 6) ]
      [ (struct float2 14 0) (struct float2 16 4) ]
      [ (struct float2 15 0) (struct float2 16 2) ]
      [ (struct float2 0 10) (struct float2 7 11) ]
      [ (struct float2 9 12) (struct float2 10 10) ]
      [ (struct float2 10 10) (struct float2 12 12) ]
      [ (struct float2 12 12) (struct float2 9 12) ]
      [ (struct float2 8 15) (struct float2 9 13) ]
      [ (struct float2 9 13) (struct float2 11 15) ]
      [ (struct float2 11 15) (struct float2 8 15) ]
      [ (struct float2 0 12) (struct float2 3 13) ]
      [ (struct float2 3 13) (struct float2 7 15) ]
      [ (struct float2 7 15) (struct float2 8 16) ]
      [ (struct float2 2 16) (struct float2 3 13) ]
      [ (struct float2 4 16) (struct float2 5 14) ]
      [ (struct float2 6 16) (struct float2 7 15) ]]))

(def *r*
  (grid 16 16 
    [
      [ (struct float2 0 12) (struct float2 1 14) ]
      [ (struct float2 0 8) (struct float2 2 12) ]
      [ (struct float2 0 4) (struct float2 5 10) ]
      [ (struct float2 0 0) (struct float2 8 8) ]
      [ (struct float2 1 1) (struct float2 4 0) ]
      [ (struct float2 2 2) (struct float2 8 0) ]
      [ (struct float2 3 3) (struct float2 8 2) ]
      [ (struct float2 8 2) (struct float2 12 0) ]
      [ (struct float2 5 5) (struct float2 12 3) ]
      [ (struct float2 12 3) (struct float2 16 0) ]
      [ (struct float2 0 16) (struct float2 2 12) ]
      [ (struct float2 2 12) (struct float2 8 8) ]
      [ (struct float2 8 8) (struct float2 14 6) ]
      [ (struct float2 14 6) (struct float2 16 4) ]
      [ (struct float2 6 16) (struct float2 11 10) ]
      [ (struct float2 11 10) (struct float2 16 6) ]
      [ (struct float2 11 16) (struct float2 12 12) ]
      [ (struct float2 12 12) (struct float2 16 8) ]
      [ (struct float2 12 12) (struct float2 16 16) ]
      [ (struct float2 13 13) (struct float2 16 10) ]
      [ (struct float2 14 14) (struct float2 16 12) ]
      [ (struct float2 15 15) (struct float2 16 14) ]]))

(def *s* 
  (grid 16 16 
    [
      [ (struct float2 0 0) (struct float2 4 2) ]
      [ (struct float2 4 2) (struct float2 8 2) ]
      [ (struct float2 8 2) (struct float2 16 0) ]
      [ (struct float2 0 4) (struct float2 2 1) ]
      [ (struct float2 0 6) (struct float2 7 4) ]
      [ (struct float2 0 8) (struct float2 8 6) ]
      [ (struct float2 0 10) (struct float2 7 8) ]
      [ (struct float2 0 12) (struct float2 7 10) ]
      [ (struct float2 0 14) (struct float2 7 13) ]
      [ (struct float2 8 16) (struct float2 7 13) ]
      [ (struct float2 7 13) (struct float2 7 8) ]
      [ (struct float2 7 8) (struct float2 8 6) ]
      [ (struct float2 8 6) (struct float2 10 4) ]
      [ (struct float2 10 4) (struct float2 16 0) ]
      [ (struct float2 10 16) (struct float2 11 10) ]
      [ (struct float2 10 6) (struct float2 12 4) ]
      [ (struct float2 12 4) (struct float2 12 7) ]
      [ (struct float2 12 7) (struct float2 10 6) ]
      [ (struct float2 13 7) (struct float2 15 5) ]
      [ (struct float2 15 5) (struct float2 15 8) ]
      [ (struct float2 15 8) (struct float2 13 7) ]
      [ (struct float2 12 16) (struct float2 13 13) ]
      [ (struct float2 13 13) (struct float2 15 9) ]
      [ (struct float2 15 9) (struct float2 16 8) ]
      [ (struct float2 13 13) (struct float2 16 14) ]
      [ (struct float2 14 11) (struct float2 16 12) ]
      [ (struct float2 15 9) (struct float2 16 10) ]]))

(def *t*
  (quartet *p* *q* *r* *s*))
  
(def *u*
  (p-cycle (rot *q*)))

(def *side1*
  (quartet (blank) (blank) (rot *t*) *t*))
  
(def *side2*
  (quartet *side1* *side1* (rot *t*) *t*))
  
(def *corner1*
  (quartet (blank) (blank) (blank) *u*))
  
(def *corner2*
  (quartet *corner1* *side1* (rot *side1*) *u*))
  
(def *pseudocorner* 
  (quartet *corner2* *side2* (rot *side2*) (rot *t*)))

(def *fishes*
  (p-cycle *pseudocorner*))