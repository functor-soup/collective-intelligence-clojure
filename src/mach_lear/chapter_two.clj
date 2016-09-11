(ns mach-lear.chapter-two
  (:require [clojure.set :as set]
            [mach-lear.metric :as metric]
            [mach-lear.utility :as utility]))

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


(def data-for-reviewer-recommendation
  (let [movie-to-int (into {} (map vector movies (iterate inc 0)))
        critics-names (keys critics)
        critics_x (fn [x] (map #(nth % x) (vals critics)))]
    (reduce-kv (fn [m k v]
                 (assoc m k (into {} (->> (map vector  critics-names (critics_x v))
                                          (remove #(nil? (second %))))))) {} movie-to-int)))


(defn sim-distance
  [& {:keys [metric] :or {metric metric/pearson-score}}]
  (fn [person1-map person2-map]
    (let [common (utility/get-common-keys person1-map person2-map)
          person1_filtered (vals  (select-keys person1-map common))
          person2_filtered (vals  (select-keys person2-map common))]
          (metric person1_filtered person2_filtered)
       )))


(def similarity-pearson   (sim-distance))
(def similarity-euclidean (sim-distance :metric metric/modified-euclidean))
(def similarity-manhattan (sim-distance :metric metric/manhattan-distance))
(def similarity-jaccard   (sim-distance :metric metric/jaccard-similarity))


(defn- get-similarity-map
  "giet map of similarities to a person based on a metric"
  [data person & {:keys [similarity] :or {similarity similarity-pearson}}]
  (let [rest (set/difference (set (keys data)) #{person})
        person-map (data person)]
      (->> rest
           (map #(->[%,(similarity (data %) person-map)]))
           (into {}))))


(defn top-matches
  [prefs person & {:keys [n similarity] :or {n 5 similarity similarity-pearson}}]
  (->> (get-similarity-map prefs person :metric similarity)
         (sort-by val >)
         (take n)))


(defn get-recommendations
  [data list-of-items person & {:keys [similarity] :or {similarity similarity-pearson}}]
  (let [others-similarity  (get-similarity-map data person :similarity similarity)
        items   (set/difference (set list-of-items) (set (keys (data person))))
        totals  (utility/make-hash-map items 0)
        simSum  (utility/make-hash-map items 0)]
    (->> (reduce-kv
          (fn [m k v]
            (-> m
              (assoc :totals (utility/map-union #(+ %1 (* v %2)) (m :totals) (data k)))
              (assoc :simSum (utility/map-union + (m :simSum) (utility/make-hash-map (keys (data k)) v)))))
          {:totals totals :simSum simSum} others-similarity)
        (vals)
        (map #(select-keys % items))
        (apply utility/map-intersection / )
        (sort-by val >))))

