(ns com.phronemophobic.clj-webgpu.d3.matrix-util
  (:refer-clojure :exclude [identity])
  (:require [clojure.core.matrix :as mat]))

(defn perspective-fov 
  ([fovy aspect-ratio near-plane far-plane]
   (let [f (/ 1.0 (Math/tan (/ fovy 2.0)))
         nf (/ 1.0 (- near-plane far-plane))]
     [[(/ f aspect-ratio) 0 0 0]
      [0 f 0 0]
      [0 0 (* (+ far-plane near-plane) nf) (* 2 far-plane near-plane nf)]
      [0 0 -1 0]])))

(defn translate [tx ty tz]
  [[1 0 0 tx]
   [0 1 0 ty]
   [0 0 1 tz]
   [0 0 0 1]])

(defn identity []
  [[1 0 0 0]
   [0 1 0 0]
   [0 0 1 0]
   [0 0 0 1]])

(defn rotate [theta-x theta-y theta-z]
  (let [cx (Math/cos theta-x)
        sx (Math/sin theta-x)
        cy (Math/cos theta-y)
        sy (Math/sin theta-y)
        cz (Math/cos theta-z)
        sz (Math/sin theta-z)]
    [[(* cy cz) (- (* cz sx sy) (* cx sz)) (+ (* sx sz) (* cx sy cz)) 0]
     [(* cy sz) (+ (* cx cz) (* sx sy sz)) (- (* cx sy sz) (* sx cz) ) 0]
     [(- sy) (* sx cy) (* cx cy) 0]
     [0 0 0 1]]))

(defn scale [sx sy sz]
  [[sx 0 0 0]
   [0 sy 0 0]
   [0 0 sz 0]
   [0 0 0  1]])

(defn ortho-mat [left right bottom top near far]
  [[(/ 2 (- right left))  0 0 0]
   [0 (/ 2 (- top bottom)) 0 0]
   [0 0 (/ 1 (- near far)) 0]
   [(/ (+ right left) (- left right)) (/ (+ top bottom) (- bottom top)) (/ near (- near far)) 1]])
