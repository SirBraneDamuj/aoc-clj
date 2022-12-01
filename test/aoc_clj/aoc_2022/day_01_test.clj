(ns aoc-clj.aoc-2022.day-01-test
  (:require [aoc-clj.aoc-2022.day-01 :as day-01]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day01.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 24000 (day-01/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 69177 (day-01/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 45000 (day-01/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 207456 (day-01/part-2 real-input)))))
