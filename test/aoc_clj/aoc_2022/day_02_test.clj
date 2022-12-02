(ns aoc-clj.aoc-2022.day-02-test
  (:require [aoc-clj.aoc-2022.day-02 :as day-02]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "A Y
B X
C Z")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day02.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 15 (day-02/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 9177 (day-02/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 12 (day-02/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 12111 (day-02/part-2 real-input)))))
