(ns mach-lear.utility
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn get-common-keys
  "given asm many hash-maps get
  the common keys as a set"
  [& args]
  (let [maps (map #(set (keys %)) args)]
    (apply set/intersection maps)))

(defn make-hash-map
  ;; given a vector of values and a default value
  ;; return a hash-map of keys with default valus
  [keys default]
  (reduce (fn [a x] (assoc a x default)) {} keys))

(defn map-difference
  "map version for set difference"
  ([] {})
  ([& args]
     (let [main-map   (nth args 0)
           difference (apply set/difference (map #(set (keys %)) args))]
      (reduce (fn [a x]
                (assoc a x (main-map x)))
              {} difference))))

(defn map-intersection
  "f over values for map intersection"
  ([] {})
  ([arg] arg)
  ([f & args]
    (let [common (apply get-common-keys args)]
      (reduce (fn [a x]
                (assoc a x (apply f (map #(% x) args))))
              {} common))))

(defn map-union
  "combines all maps into one
  big fat map"
  ([] {})
  ([arg] arg)
  ([f & args]
    (let [keys (->> args
                  (map keys) (flatten)
                  (remove nil?) (distinct))]
      (reduce (fn [a x]
                (let [k (remove nil? (map #(% x) args))]
                  (if (> (count k) 1)
                    (assoc a x (apply f k))
                    (assoc a x (nth k 0))))) {} keys))))

(defn map-xor
  "gives back a vector of maps with non-common
  key-value pairs"
  ([] {})
  ([& args]
    (if (= (count (distinct args)) 1)
      (take (count args) (repeat {}))
      (let [permutations (combo/permutations args)]
        (distinct (reduce (fn [a x]
                            (conj a (apply map-difference x)))
                          [] permutations))))))
