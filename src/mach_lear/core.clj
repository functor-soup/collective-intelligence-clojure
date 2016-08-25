(ns mach-lear.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.set :as set]))

;; forming the data, doing it functionally
;; because repetition is boring
(defonce movies ["Lady in the Water","Snakes on a Plane", "Just My Luck",
                 "Superman Returns", "You, Me and Dupree", "The Night Listener"])

(defonce critics {"Lisa Rose"    [2.5 3.5 3.0 3.5 2.5 3.0]
                  "Gene Seymour" [3.0 3.5 1.5 5.0 3.5 3.0]
                  "Michael Phillips" [2.5 3.0 nil 3.0 3.5 nil]
                  "Claudia Puig" [nil 3.5 3.0 4.0 2.5 4.5]
                  "Mike LaSalle" [3.0 4.0 2.0 3.0 2.0 3.0]
                  "Jack Matthews" [3.0 4.0 nil 5.0 3.5 3.0]
                  "Toby" [nil 4.5 nil 4.0 1.0 nil]
                  })

;; todo: refactor this
(def data (letfn ([clean [x]
                   (->> x
                     (filter #(some? (second %)))
                     (into {}))])
            (->> critics
                (map #(-> [(first %) (clean (zipmap movies (second %)))]))
                (into {}))))


(defn map-keys-intersection [map1 map2]
  (let [map1_ (set (keys map1))
        map2_ (set (keys map2))]
    (into [](set/intersection map1_ map2_))
    ))

(defn metric-one [zippedTuples]
  (->> zippedTuples
       (map #(math/expt (- (first %) (second %)) 2))
       (apply +)
       (+ 1)
       (/ 1)))

(defn pearson-score [zippedTuples]
  (let [index_1 (map first zippedTuples)
        index_2 (map second zippedTuples)
        sum1 (apply + index_1)
        sum2 (apply + index_2)
        sum1sq (apply + (map #(math/expt % 2) index_1))
        sum2sq (apply + (map #(math/expt % 2) index_2))
        psum (apply + (map * index_1 index_2))
        num (- psum (/ (* sum1 sum2) (count index_1)))
        den_1 (- sum1sq (/ (math/expt sum1 2) (count index_1)))
        den_2 (- sum2sq (/ (math/expt sum2 2) (count index_1)))
        den (math/expt (* den_1 den_2) 0.5)
        ]
    (cond
      (= den 0 ) 0)
      :else (/ num den)
      ))

(defn manhattan-distance [tupleList]
  (->> tupleList
       (map #(math/abs (- (first %) (second %))))
       (apply + )))

(defn sim-distance
  [data person1 person2 & {:keys [metric] :or {metric pearson-score}}]
  (let [person1_ (data person1)
        person2_ (data person2)
        common (map-keys-intersection person1_ person2_)
        person1_filtered (vals  (select-keys person1_ common))
        person2_filtered (vals  (select-keys person2_ common))
        input (map vector person1_filtered person2_filtered)]
       (metric input)
    ))

(defn jaccard-similarity
  [tupleList]
  (let [minStuff (map #(apply min %) tupleList)
        maxStuff (map #(apply max %) tupleList)]
     (/ (apply + minStuff) (apply + maxStuff))
    ))

(defn topMatches
  [prefs person & {:keys [n similarity] :or {n 5 similarity sim-distance}}]
  (let [ rest (set/difference (set (keys prefs)) #{person}) ]
    (->> rest
         (map #(->[%,(similarity prefs % person :metric jaccard-similarity)]))
         (into {})
         (sort-by val >)
         (take n)
    )))


;;(println (sim-distance data "Lisa Rose" "Gene Seymour"))
(println (topMatches data "Toby"))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
