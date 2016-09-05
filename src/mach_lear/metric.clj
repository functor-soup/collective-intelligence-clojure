(ns mach-lear.metric
  (:require [clojure.math.numeric-tower :as math]
            [clojure.set :as set]))

(defn modified-euclidean [point1 point2]
  (->> (map vector point1 point2)
       (map #(math/expt (- (first %) (second %)) 2))
       (apply +)
       (#(math/expt % 0.5))
       (+ 1)
       (/ 1)))

(defn pearson-score
  [point1 point2]
  (let [sum1 (apply + point1)
        sum2 (apply + point2)
        sum1sq (apply + (map #(math/expt % 2) point1))
        sum2sq (apply + (map #(math/expt % 2) point2))
        psum (apply + (map * point1 point2))
        num (- psum (/ (* sum1 sum2) (count point1)))
        den_1 (- sum1sq (/ (math/expt sum1 2) (count point1)))
        den_2 (- sum2sq (/ (math/expt sum2 2) (count point1)))
        den (math/expt (* den_1 den_2) 0.5)
        ]
    (cond
      (= den 0 ) 0)
      :else (/ num den)
      ))

(defn manhattan-distance
  [point1 point2]
  (->> (map vector point1 point2)
       (map #(math/abs (- (first %) (second %))))
       (apply + )))

;; todo: research this concept more
(defn jaccard-similarity
  [point1 point2]
  (let [zipped   (map vector point1 point2)
        minStuff (map #(apply min %) zipped)
        maxStuff (map #(apply max %) zipped)]
     (/ (apply + minStuff) (apply + maxStuff))
    ))

