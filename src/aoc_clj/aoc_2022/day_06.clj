(ns aoc-clj.aoc-2022.day-06
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

(def sample-input
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defn find-marker
  [s current-marker found-chars]
  (pprint current-marker)
  (if (or (= 4 (count current-marker))
          (not (seq s)))
    current-marker
    (let [next-char (first s)]
      (if (or (some #(= % next-char) current-marker)
              (contains? found-chars next-char))
        (find-marker (rest s) (drop 1 current-marker) (into found-chars current-marker))
        (find-marker (rest s) (conj current-marker next-char) found-chars)))))



#_(defn part-1
    [input]
    (let [candidates (partition 4 1 input)
          marker (reduce (fn [[it found-chars] candidate]
                           (if it
                             [it found-chars]
                             (let [these-chars (set candidate)
                                   less-prev (set/difference these-chars found-chars)]
                               (if (= 4 (count less-prev))
                                 [candidate found-chars]
                                 [nil (conj found-chars (first candidate))]))))
                         [nil #{}]
                         candidates)]
      (pprint marker)
      (pprint candidates)
      (str/index-of input (str/join marker))))

(defn part-1
  [input]
  (let [candidates (partition 4 1 input)
        marker (some (fn [candidate]
                       (when (= 4 (count (set candidate)))
                         candidate))
                     candidates)]
    (+ 4 (str/index-of input (str/join marker)))))

(defn part-2
  [input]
  (let [candidates (partition 14 1 input)
        marker (some (fn [candidate]
                       (when (= 14 (count (set candidate)))
                         candidate))
                     candidates)]
    (+ 14 (str/index-of input (str/join marker)))))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 6))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 6)))
