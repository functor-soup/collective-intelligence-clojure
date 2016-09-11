(ns mach-lear.utility-test
  (:require [clojure.test :refer :all]
            [mach-lear.utility :refer :all]))

(deftest map-intersection-test
  (let [map-a      {"alpha" 2 "beta" 4}
        map-b      {"alpha" 5 "beta" 7 "gamma" 6}
        result-sum {"alpha" 7 "beta" 11}]
    (is (= result-sum (map-intersection + map-a map-b)))))

(deftest map-intersection-empty-test
  (is (= {} (map-intersection {}))))

(deftest map-intersection-empty-test-2
  (is (= {} (map-intersection + {}))))

(deftest map-intersection-empty-test-3
  (is (= {} (map-intersection + {} {:alpha 2}))))

(deftest map-intersection-empty-test-4
  (is (= {} (map-intersection + {} {}))))

(deftest get-common-keys-test
  (let [map-a {:omega 3 :tyri 4 :polo 4}
        map-b {:omega 5 :tyri 8}
        result #{:omega :tyri}]
     (is (= result (get-common-keys map-a map-b)))
    ))

(deftest make-hash-map-test-a
  (let [map-a {:alpha [] :beta []}]
    (is (= (make-hash-map [:alpha :beta] []) map-a))))

(deftest make-hash-map-test-b
  (let [map-a {:alpha [] :beta []}]
    (is (= (make-hash-map #{:alpha :beta} []) map-a))))

(deftest map-union-test
  (let [map-a {:alpha 3 :beta 4 :ceta 4}
        map-b {:agatha 45}
        map-c {:beta 45 :gamma 78}
        result {:alpha 3 :beta 49 :ceta 4 :gamma 78 :agatha 45}]
  (is (= result (map-union + map-a map-c map-b)))))

(deftest map-union-empty-test
  (is (= {} (map-union {}))))

(deftest map-union-empty-test-2
  (is (= {} (map-union + {} {}))))

(deftest map-intersection-empty-test-3
  (let [map-a {:alpha 2}]
    (is (= map-a (map-union + {} map-a)))))

(deftest map-difference-test-a
  (let [map-a  {:gamma 12 :golang 23 :jimmy 7}
        map-b  {:gamma 23}
        map-c  {:gamma 2 :jimmy 8}
        result {:golang 23}]
    (is (= result (map-difference map-a map-b map-c)))))

(deftest map-difference-test-b
  (let [map-a  {:gamma 12 :golang 23 :jimmy 7}
        map-b  {:gamma 23 :golang 1}
        map-c  {:gamma 2 :jimmy 8}
        result {}]
    (is (= result (map-difference map-a map-b map-c)))))

(deftest map-difference-empty-test
  (is (= {} (map-difference {}))))

(deftest map-difference-empty-test-2
  (is (= {} (map-difference {} {:alpha "omega"}))))

(deftest map-difference-empty-test-3
  (is (= {} (map-difference {} {}))))

(deftest map-difference-empty-test-4
  (let [map-a {:gamma "gamma"}]
    (is (= map-a (map-difference map-a {})))))

(deftest map-xor-test
  (let [map-a  {:gamma 12 :golang 23 :jimmy 7}
        map-b  {:gamma 23}
        map-c  {:gamma 2 :jimmy 8 :daffy 7}
        result [{:golang 23} {} {:daffy 7}]]
        (is (= result (map-xor map-a map-b map-c)))))

(deftest map-xor-empty-test
  (is (= '({}) (map-xor {}))))

(deftest map-xor-empty-test-2
  (is (= '({} {}) (map-xor {} {}))))

(deftest map-xor-empty-test-3
  (let [map-a {:alpha "alpha" :beta "beta"}]
     (is (= (seq [map-a {}]) (map-xor map-a {})))))

(deftest map-xor-empty-test-4
  (let [map-a {:alpha "alpha" :beta "beta"}]
    (is (= (seq [{} {}]) (map-xor map-a map-a)))))
