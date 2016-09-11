(ns mach-lear.chapter-two-test
  (:require [clojure.test :refer :all]
            [mach-lear.chapter-two :refer :all]))

(deftest recommend-critics-test
  (let [data data-for-reviewer-recommendation
        result  (get-recommendations
                 data-for-reviewer-recommendation (keys critics) "Superman Returns")]
    (is (= true (empty? result)))))

;; potential bug somewhere
(deftest recommend-critics-test-2
  (let [data data-for-reviewer-recommendation
        result  (get-recommendations
                 data-for-reviewer-recommendation (keys critics) "Just My Luck")]
    (is (= (seq ["Jack Matthews" "Toby"]) (take 2 (keys result))))))

(deftest top-matches-test
  (let [result (keys (top-matches data "Toby" :n 3))]
    (is (= (seq ["Lisa Rose" "Mike LaSalle" "Claudia Puig"]) result))))

(deftest recommend-movies-test
  (let [result  (get-recommendations data movies "Toby")]
    (is (= (seq ["The Night Listener" "Lady in the Water" "Just My Luck"]) (take 3 (keys result))))))

