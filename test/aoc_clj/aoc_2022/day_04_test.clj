(ns aoc-clj.aoc-2022.day-04-test
  (:require [aoc-clj.aoc-2022.day-04 :as day-04]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day04.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 2 (day-04/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 448 (day-04/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 4 (day-04/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 794 (day-04/part-2 real-input)))))
