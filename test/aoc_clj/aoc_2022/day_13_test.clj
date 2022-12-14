(ns aoc-clj.aoc-2022.day-13-test
  (:require [aoc-clj.aoc-2022.day-13 :as day-13]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day13.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 13 (day-13/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 6478 (day-13/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 140 (day-13/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 21922 (day-13/part-2 real-input)))))
