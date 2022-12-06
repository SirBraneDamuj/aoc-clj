(ns aoc-clj.aoc-2022.day-06
  (:require [clojure.string :as str]))

(defn find-marker-end-pos
  [s size]
  (let [marker (->> s
                    (partition size 1)
                    (some #(let [valid? (-> %
                                            set
                                            count
                                            (= size))]
                             (when valid? %)))
                    str/join)]
    (+ size (str/index-of s marker))))

(defn part-1
  [input]
  (find-marker-end-pos input 4))

(defn part-2
  [input]
  (find-marker-end-pos input 14))

(def solution
  {:year 2022
   :day 6
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
