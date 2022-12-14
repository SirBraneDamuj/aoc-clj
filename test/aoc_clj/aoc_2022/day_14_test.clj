(ns aoc-clj.aoc-2022.day-14-test
  (:require [aoc-clj.aoc-2022.day-14 :as day-14]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day14.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 24 (day-14/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 763 (day-14/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 93 (day-14/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 23921 (day-14/part-2 real-input)))))
