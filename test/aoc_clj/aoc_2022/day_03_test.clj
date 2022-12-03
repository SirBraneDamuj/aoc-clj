(ns aoc-clj.aoc-2022.day-03-test
  (:require [aoc-clj.aoc-2022.day-03 :as day-03]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day03.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 157 (day-03/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 8105 (day-03/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 70 (day-03/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 2363 (day-03/part-2 real-input)))))
