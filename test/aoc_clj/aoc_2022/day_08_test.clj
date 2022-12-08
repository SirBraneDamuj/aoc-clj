(ns aoc-clj.aoc-2022.day-08-test
  (:require [aoc-clj.aoc-2022.day-08 :as day-08]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "30373
25512
65332
33549
35390")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day08.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 21 (day-08/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 1827 (day-08/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 8 (day-08/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 335580 (day-08/part-2 real-input)))))
