(ns aoc-clj.aoc-2022.day-05-test
  (:require [aoc-clj.aoc-2022.day-05 :as day-05]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day05.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= "CMZ" (day-05/part-1 test-input))))
  (testing "satisfies real input"
    (is (= "HNSNMTLHQ" (day-05/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= "MCD" (day-05/part-2 test-input))))
  (testing "satisfies real input"
    (is (= "RNLFDJMCT" (day-05/part-2 real-input)))))
