(ns com.phronemophobic.clj-webgpu.camera.orbit
  (:require [com.phronemophobic.clj-webgpu.d3.matrix-util :as mutil]
            [clojure.core.matrix :as mat]))

;; reference
;; https://webgpufundamentals.org/webgpu/lessons/webgpu-camera-controls.html

(defn make 
  ([]
   (make nil))
  ([{:keys [target pan tilt radius]}]
   {:type :camera/orbit
    :target (or target
                [0 0 0])
    :pan (or pan 0)
    :tilt (or tilt 0)
    :radius (or radius 0)}))

(defn ->matrix [camera]
  (let [{:keys [target pan tilt radius]} camera]
    (mat/mmul
     (apply mutil/translate target)
     (mutil/rotate tilt pan 0)
     (mutil/translate 0 0 radius))))

(defmethod mutil/->matrix :camera/orbit [m]
  (->matrix m))

;; track is moving the camera perpendicular to the way it’s facing.
(defn track [camera [dx dy]]
  (let [camera-direction (mutil/transform-mat3 [dx dy 0]
                                               (->matrix camera))]

    (update camera :target #(mapv + % camera-direction))))

;; "Dolly" is moving the camera closer or further
(defn dolly [camera delta]
  (update camera :radius + delta))

(defn pan [camera dp]
  (update camera :pan + dp))

(defn tilt [camera dt]
  (update camera :tilt + dt))

