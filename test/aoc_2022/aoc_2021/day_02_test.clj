(ns aoc-2022.aoc-2021.day-02-test
  (:require [aoc-2022.aoc-2021.day-02 :as day-02]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def sample-input
  "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(def real-input
  (slurp (io/file "resources" "real-input" "2021" "day02.txt")))

(deftest part-1
  (testing "satisfies sample input"
    (is (= 150 (day-02/part-1 sample-input))))
  (testing "satisfies real input"
    (is (= 1813801 (day-02/part-1 real-input)))))

(deftest part-2
  (testing "satisfies sample input"
    (is (= 900 (day-02/part-2 sample-input))))
  (testing "satisfies real input"
    (is (= 1960569556 (day-02/part-2 real-input)))))
