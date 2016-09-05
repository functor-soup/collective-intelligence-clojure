(ns mach-lear.metric-test
  (:require [clojure.test :refer :all]
            [mach-lear.metric :refer :all]))

(deftest manhattan-distance-test
  (let [point-a    [1 4 12 3]
        point-b    [3 4 12 100]
        result-sum 99]
    (is (= result-sum (manhattan-distance point-a point-b)))))

(deftest pearson-score-test
  (let [point-a [1 12 3 34 100 89]
        point-b [8 12 10 12 23 56]
        result-max 0.8
        result-min 0.7
        result (pearson-score point-a point-b)]
    (is (and (> result result-min) (> result-max result)))))

(deftest modified-euclidean-test
  (let [point-a [23 34 56 8]
        point-b [12 7 9 78]
        result   (modified-euclidean point-a point-b)]
    (is (> 0.13 result) (> result 0.10))))
