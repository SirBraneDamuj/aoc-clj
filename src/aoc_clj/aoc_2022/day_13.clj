(ns aoc-clj.aoc-2022.day-13
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]
            [clojure.pprint :refer [pprint]]))

(def sample-input
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

(defn compare-values
  [x y]
  (let [result
        (cond
          (and (int? x)
               (int? y))
          (cond
            (= x y)
            nil
            (< x y)
            :pass
            :else
            :fail)

          (and (vector? x)
               (vector? y))
          (let [idxs (range (count x))]
            (or (some (fn [idx]
                        (if (>= idx (count y))
                          :fail
                          (compare-values (nth x idx) (nth y idx))))
                      idxs)
                (if (= (count x) (count y))
                  nil
                  :pass)))

          :else
          (let [x-vec (if (vector? x) x [x])
                y-vec (if (vector? y) y [y])]
            (compare-values x-vec y-vec)))]
    #_(pprint [x y result])
    result))

(defn parse-packet
  [packet-line]
  (->> packet-line
       str/split-lines
       (map read-string)))

(defn parse-input
  [input]
  (->> input
       util/split-newline-delim-line-groups
       (map parse-packet)))

(defn part-1
  [input]
  (->> input
       parse-input
       (map-indexed (fn [idx [x y]]
                      (if (= :pass (compare-values x y))
                        (inc idx)
                        0)))
       util/ppeek
       (apply +)))

(defn packet-comp
  [x y]
  (let [result (compare-values x y)]
    (cond
      (= :pass result)
      -1
      (= :fail result)
      1
      :else
      0)))

(defn part-2
  [input]
  (->> input
       parse-input
       (apply concat)
       (#(conj % [[6]] [[2]]))
       (sort packet-comp)
       (#(* (inc (.indexOf % [[6]]))
            (inc (.indexOf % [[2]]))))))

(def solution
  {:year 2022
   :day 13
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 13))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 13)))
