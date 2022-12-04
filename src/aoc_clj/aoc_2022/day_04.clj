(ns aoc-clj.aoc-2022.day-04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-range
  [range-str]
  (let [[left right] (map parse-long (str/split range-str #"-"))]
    (set (range left (inc right)))))

(defn parse-pair
  [pair-str]
  (let [[left right] (map parse-range (str/split pair-str #","))
        x (or (set/superset? left right) (set/superset? right left))]
    x))

(defn parse-any-overlap
  [pair-str]
  (let [[left right] (map parse-range (str/split pair-str #","))
        x (empty? (set/intersection left right))]
    (not x)))

(defn part-1
  [input]
  (let [lines (str/split-lines input)
        overlapping-pairs (map parse-pair lines)]
    (count (filter identity overlapping-pairs))))

(defn part-2
  [input]
  (let [lines (str/split-lines input)
        overlapping-pairs (map parse-any-overlap lines)]
    (count (filter identity overlapping-pairs))))

(comment
  (require '[aoc-clj.core :as aoc])

  (let [r (range 3 8)]
    (set r))

  (let [foo (set (range 92 92))
        bar (set (range 90 91))]
    (set/superset? bar foo))

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 4))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 4)))
