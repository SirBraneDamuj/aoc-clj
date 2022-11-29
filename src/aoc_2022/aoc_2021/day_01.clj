(ns aoc-2022.aoc-2021.day-01
  (:require [clojure.string :as str]))

(defn part-1
  [input]
  (->> (str/split-lines input)
       (map parse-long)
       (partition 2 1)
       (filter (fn [[x y]] (> y x)))
       count))

(defn part-2
  [input]
  (->> (str/split-lines input)
       (map parse-long)
       (partition 3 1)
       (map #(reduce + %))
       (partition 2 1)
       (filter (fn [[x y]] (> y x)))
       count))

(def solution
  {:year 2021
   :day 1
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-2022.core :as aoc])
  (aoc/run-solution solution))

