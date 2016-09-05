(ns mach-lear.chapter-two
  (:require [clojure.set :as set]
            [mach-lear.metric :as metric]))

;; forming the data, doing it functionally
;; because repetition is boring
(defonce movies ["Lady in the Water","Snakes on a Plane", "Just My Luck",
                 "Superman Returns", "You, Me and Dupree", "The Night Listener"])

(defonce critics {"Lisa Rose"        [2.5 3.5 3.0 3.5 2.5 3.0]
                  "Gene Seymour"     [3.0 3.5 1.5 5.0 3.5 3.0]
                  "Michael Phillips" [2.5 3.0 nil 3.0 3.5 nil]
                  "Claudia Puig"     [nil 3.5 3.0 4.0 2.5 4.5]
                  "Mike LaSalle"     [3.0 4.0 2.0 3.0 2.0 3.0]
                  "Jack Matthews"    [3.0 4.0 nil 5.0 3.5 3.0]
                  "Toby"             [nil 4.5 nil 4.0 1.0 nil]
                  })

;; todo: refactor this
(def data (letfn ([clean [x]
                   (->> x
                     (filter #(some? (second %)))
                     (into {}))])
            (->> critics
                (map #(-> [(first %) (clean (zipmap movies (second %)))]))
                (into {}))))

(defn sim-distance
  [& {:keys [metric] :or {metric metric/pearson-score}}]
  (fn [data person1 person2]
    (let [person1_ (data person1)
        person2_ (data person2)
        common (map-keys-intersection person1_ person2_)
        person1_filtered (vals  (select-keys person1_ common))
        person2_filtered (vals  (select-keys person2_ common))
        input (map vector person1_filtered person2_filtered)]
       (apply metric input)
    )))

(def similarity-pearson   (sim-distance))
(def similarity-euclidean (sim-distance metric/modified-euclidean))
(def similarity-manhattan (sim-distance metric/manhattan-distance))
(def similarity-jaccard   (sim-distance metric/jaccard-similarity))


(defn topMatches
  [prefs person & {:keys [n similarity] :or {n 5 similarity similarity-pearson}}]
  (let [ rest (set/difference (set (keys prefs)) #{person}) ]
    (->> rest
         (map #(->[%,(similarity prefs % person)]))
         (into {})
         (sort-by val >)
         (take n)
    )))

(defn getRecommendations
  [data person & {:keys [similarity] :or {similarity sim-distance}}]
  (let [others (set/difference (set (keys data)) #{person})
        others-similarity (into {} (map #(-> [%,(sim-distance data person %)]) others)) ; map of person -> similarity 
        items (keys (data person))
        totals (make-hash-map items 0)
        simSum (make-hash-map items 0)]
    (reduce-kv (fn [m k v] (do  (map-operation-on-vals + (:totals m) (k data))))
                     {:totals totals :simSum simSum} others-similarity )))


;;(println (sim-distance data "Lisa Rose" "Gene Seymour"))
(getRecommendations data "Toby")




