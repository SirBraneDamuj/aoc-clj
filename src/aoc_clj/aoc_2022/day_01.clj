(ns aoc-clj.aoc-2022.day-01
  (:require [clojure.string :as str]))

(defn elf-calories
  [groups]
  (let [elf-snacks (map #(map parse-long (str/split-lines %)) groups)]
    (map #(reduce + %) elf-snacks)))

(defn part-1
  [input]
  (let [groups (str/split input #"\n\n")]
    (apply max (elf-calories groups))))

(defn part-2
  [input]
  (let [groups (str/split input #"\n\n")
        elf-cals-sorted (sort (elf-calories groups))
        top-3 (take-last 3 elf-cals-sorted)]
    (apply + top-3)))

(def solution
  {:year 2022
   :day 1
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
