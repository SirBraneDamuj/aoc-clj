(ns aoc-clj.aoc-2022.day-13
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]
            [clojure.edn :as edn]))

(defn vec-wrap
  [x-or-xs]
  (if (vector? x-or-xs)
    x-or-xs
    [x-or-xs]))

(defn compare-values
  [x y]
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
    (let [x-vec (vec-wrap x)
          y-vec (vec-wrap y)]
      (compare-values x-vec y-vec))))

(defn parse-packet
  [packet-line]
  (->> packet-line
       str/split-lines
       (map edn/read-string)))

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
  (let [divider-1 [[6]]
        divider-2 [[2]]]
    (->> input
         parse-input
         (apply concat)
         (#(conj % divider-1 divider-2))
         (sort packet-comp)
         (#(* (inc (.indexOf % divider-1))
              (inc (.indexOf % divider-2)))))))

(def solution
  {:year 2022
   :day 13
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
