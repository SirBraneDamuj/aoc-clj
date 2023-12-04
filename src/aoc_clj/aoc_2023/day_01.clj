(ns aoc-clj.aoc-2023.day-01
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]))

(def reg
  #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))")

(def mapping
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(defn translate-digit
  [s]
  (if-let [x (mapping s)]
    x
    s))

(defn ->code
  [s]
  (as-> s $
    (re-seq reg $)
    (map #(-> % second translate-digit) $)
    (str (first $) (last $))
    (Integer/parseInt $)))

(defn part-1
  [input]
  (as-> input $
    (str/split $ #"\n")
    (map #(str/replace % #"\D" "") $)
    (map ->code $)
    (reduce + $)))

(defn part-2
  [input]
  (let [lines (str/split input #"\n")
        codes (map ->code lines)]
    (reduce + codes)))

(def solution
  {:year 2023
   :day 1
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
