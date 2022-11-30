(ns aoc-2022.aoc-2021.day-03-test
  (:require [aoc-2022.aoc-2021.day-03 :as day-03]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def sample-input
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(def real-input
  (slurp (io/file "resources" "real-input" "2021" "day03.txt")))

(deftest part-1
  (testing "satisfies sample input"
    (is (= 198 (day-03/part-1 sample-input))))
  (testing "satisfies real input"
    (is (= 4174964 (day-03/part-1 real-input)))))

(deftest part-2
  (testing "satisfies sample input"
    (is (= 230 (day-03/part-2 sample-input))))
  (testing "satisfies real input"
    (is (= 4474944 (day-03/part-2 real-input)))))
