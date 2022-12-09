(ns aoc-clj.aoc-2022.day-09-test
  (:require [aoc-clj.aoc-2022.day-09 :as day-09]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def test-input-2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day09.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 13 (day-09/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 6269 (day-09/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 36 (day-09/part-2 test-input-2))))
  (testing "satisfies real input"
    (is (= 2557 (day-09/part-2 real-input)))))
