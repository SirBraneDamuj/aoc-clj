(ns aoc-2022.aoc-2021.day-01-test
  (:require [aoc-2022.aoc-2021.day-01 :as day-01]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "199
200
208
210
200
207
240
269
260
263")

(def real-input
  (slurp (io/file "resources" "real-input" "2021" "day01.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 7 (day-01/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 1451 (day-01/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 5 (day-01/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 1395 (day-01/part-2 real-input)))))
