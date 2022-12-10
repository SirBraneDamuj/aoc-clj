(ns aoc-clj.aoc-2022.day-10
  (:require [clojure.string :as str]))

(defn parse-instruction
  [line]
  (if (str/starts-with? line "addx")
    (let [[_ amount] (str/split line #" ")]
      [2 (parse-long amount)])
    [1 0]))

(defn perform-instruction
  [history [cycles arg]]
  (let [current-x (last history)]
    (if (= 2 cycles)
      (conj history current-x (+ current-x arg))
      (conj history current-x))))

(defn perform-instructions
  [input]
  (->> input
       str/split-lines
       (map parse-instruction)
       (reduce perform-instruction [1])))

(defn part-1
  [input]
  (let [register-history (perform-instructions input)]
    (->> [20 60 100 140 180 220]
         (map #(* % (nth register-history (dec %))))
         (apply +))))

(defn render-pixel
  [x register]
  (if (#{(dec x) x (inc x)} register)
    "#"
    "."))

(defn render-row
  [row]
  (str/join (map-indexed render-pixel row)))

(defn part-2
  [input]
  (->> input
       perform-instructions
       (partition 40)
       (map render-row)
       (str/join "\n")))

(def solution
  {:year 2022
   :day 10
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (part-2 (aoc/get-puzzle-input 2022 10))
  (aoc/run-solution solution))
