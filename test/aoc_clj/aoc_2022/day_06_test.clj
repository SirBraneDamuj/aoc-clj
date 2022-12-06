(ns aoc-clj.aoc-2022.day-06-test
  (:require [aoc-clj.aoc-2022.day-06 :as day-06]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
  [["mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7 19]
   ["bvwbjplbgvbhsrlpgdmjqwftvncz" 5 23]
   ["nppdvjthqldpwncqszvftbrmjlhg" 6 23]
   ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10 29]
   ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11 26]])

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day06.txt")))

(deftest part-1
  (testing "satisfies test input"
    (doseq [[s answer] test-input]
      (is (= answer (day-06/part-1 s)))))
  (testing "satisfies real input"
    (is (= 1142 (day-06/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (doseq [[s _ answer] test-input]
      (is (= answer (day-06/part-2 s)))))
  (testing "satisfies real input"
    (is (= 2803 (day-06/part-2 real-input)))))
