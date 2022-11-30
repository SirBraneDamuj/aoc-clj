(ns aoc-2022.aoc-2021.day-03
  (:require [clojure.string :as str]
            [aoc-2022.utils :as utils]))

(defn count-bits
  [counts line]
  (map-indexed
   (fn [i _]
     (let [bit (-> (nth line i)
                   str
                   keyword)]
       (update (nth counts i) bit inc)))
   counts))

(defn part-1
  [input]
  (let [lines (str/split-lines input)
        initial-counts (mapv (fn [_] {:0 0 :1 0}) (first lines))
        counts (reduce count-bits initial-counts lines)
        gamma-bin (str/join (mapv #(if (> (:0 %) (:1 %)) "0" "1") counts))
        gamma-dec (utils/parse-binary gamma-bin)
        epsilon-bin (str/join (mapv #(if (< (:0 %) (:1 %)) "0" "1") counts))
        epsilon-dec (utils/parse-binary epsilon-bin)]
    (* gamma-dec epsilon-dec)))

(defn oxy-criteria
  [{zero :0
    one :1}]
  (cond
    (= zero one)
    "1"
    (> zero one)
    "0"
    :else
    "1"))

(defn co2-criteria
  [{zero :0
    one :1}]
  (cond
    (= zero one)
    "0"
    (> zero one)
    "1"
    :else
    "0"))

(defn find-rating
  [criteria-fn lines i]
  (let [initial-counts (mapv (fn [_] {:0 0 :1 0}) (first lines))
        counts (reduce count-bits initial-counts lines)
        criterias (mapv criteria-fn counts)
        remaining (filterv #(let [bit (str (nth % i))
                                  criteria (nth criterias i)]
                              (= bit criteria)) lines)]
    (if (or (< (count remaining) 2)
            (= (inc i) (count (first lines))))
      (first remaining)
      (find-rating criteria-fn remaining (inc i)))))

(defn part-2
  [input]
  (let [lines (str/split-lines input)
        oxy-bin (find-rating oxy-criteria lines 0)
        co2-bin (find-rating co2-criteria lines 0)]
    (* (utils/parse-binary oxy-bin) (utils/parse-binary co2-bin))))

(def solution
  {:year 2021
   :day 3
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-2022.core :as aoc])
  (aoc/run-solution solution))
