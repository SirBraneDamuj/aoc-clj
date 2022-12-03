(ns aoc-clj.aoc-2022.day-03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn part-1
  [input]
  (let [lines (str/split-lines input)
        commons (map #(->> %
                           (partition (/ (count %) 2))
                           (map set)
                           (apply set/intersection)) lines)
        commons-list (reduce #(apply conj %1 %2) [] commons)
        priorities (map #(let [x (int %)]
                           (if (< x 91)
                             (+ x -65 27)
                             (+ x -97 1)))
                        commons-list)]
    (apply + priorities)))

(defn group-common
  [group]
  (let [sets (map set group)]
    (first (apply set/intersection sets))))

(defn part-2
  [input]
  (let [lines (str/split-lines input)
        groups (partition 3 lines)
        commons (map group-common groups)
        priorities (map #(let [x (int %)]
                           (if (< x 91)
                             (+ x -65 27)
                             (+ x -97 1)))
                        commons)]
    (apply + priorities)))

(comment
  (require '[aoc-clj.core :as aoc])

  (int \A)

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 3))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 3)))
