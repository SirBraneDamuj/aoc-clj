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

(defn either-pair-superset?
  [pair-str]
  (let [[left right] (map parse-range (str/split pair-str #","))]
    (or (set/superset? left right)
        (set/superset? right left))))

(defn pairs-overlap?
  [pair-str]
  (let [[left right] (map parse-range (str/split pair-str #","))]
    (seq (set/intersection left right))))

(defn part-1
  [input]
  (let [lines (str/split-lines input)]
    (count (filter either-pair-superset? lines))))

(defn part-2
  [input]
  (let [lines (str/split-lines input)]
    (count (filter pairs-overlap? lines))))

(def solution
  {:year 2022
   :day 4
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
