(ns aoc-clj.aoc-2022.day-03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn char->priority
  [c]
  (let [x (int c)]
    (if (< x 91)
      (+ x -65 27)
      (+ x -97 1))))

(defn rucksack->dupe-compartment-items
  [rucksack]
  (->> rucksack
       (partition (/ (count rucksack) 2))
       (map set)
       (apply set/intersection)
       vec))

(defn part-1
  [input]
  (let [rucksacks (str/split-lines input)
        rucksacks-dupe-items (mapcat rucksack->dupe-compartment-items rucksacks)
        priorities (map char->priority rucksacks-dupe-items)]
    (apply + priorities)))

(defn group->common-item-type
  [group]
  (let [sets (map set group)]
    (first (apply set/intersection sets))))

(defn part-2
  [input]
  (let [rucksacks (str/split-lines input)
        groups (partition 3 rucksacks)
        commons (map group->common-item-type groups)
        priorities (map char->priority commons)]
    (apply + priorities)))

(def solution
  {:year 2022
   :day 3
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
