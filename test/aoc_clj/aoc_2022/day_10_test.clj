(ns aoc-clj.aoc-2022.day-10-test
  (:require [aoc-clj.aoc-2022.day-10 :as day-10]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

;; this is just the sample input. it was pretty close to the real thing this time
(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day10.txt")))

(deftest part-1
  (testing "satisfies sample input"
    (is (= 13140 (day-10/part-1 real-input)))))

(deftest part-2
  (testing "satisfies sample input"
    (is (= "##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######....."
           (day-10/part-2 real-input)))))
