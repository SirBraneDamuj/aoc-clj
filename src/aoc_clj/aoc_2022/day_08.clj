(ns aoc-clj.aoc-2022.day-08
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]
            [clojure.pprint :refer [pprint]]))

(def sample-input
  "30373
25512
65332
33549
35390")

(defn parse-rows
  [input]
  (let [lines (str/split-lines input)]
    (mapv
     #(mapv (comp parse-long str) %)
     lines)))

(defn parse-columns
  [input]
  (let [lines (str/split-lines input)]
    (into []
          (for [x (range (count (first lines)))]
            (mapv #(parse-long (str (nth % x))) lines)))))

(def dirs
  [:up
   :down
   :left
   :right])

(defn part-1
  [input]
  (let [rows (parse-rows input)
        cols (parse-columns input)
        result
        (for [row-n (range (count rows))
              col-n (range (count cols))]
          (if (or (= 0 row-n)
                  (= 0 col-n)
                  (= (dec (count rows)) row-n)
                  (= (dec (count cols)) col-n))
            true
            (let [row (nth rows row-n)
                  col (nth cols col-n)
                  tree-height (nth (nth rows row-n) col-n)
                  b
                  (reduce
                   (fn [vis dir]
                     (let [blocked (cond
                                     (= :up dir)
                                     (some #(>= % tree-height) (subvec col 0 row-n))

                                     (= :down dir)
                                     (some #(>= % tree-height) (subvec col (inc row-n)))

                                     (= :left dir)
                                     (some #(>= % tree-height) (subvec row 0 col-n))

                                     (= :right dir)
                                     (some #(>= % tree-height) (subvec row (inc col-n))))]
                       (or vis (not blocked))))
                   false
                   dirs)]
              b)))]
    (count (filter identity result))))

(defn part-2
  [input]
  (let [rows (parse-rows input)
        cols (parse-columns input)
        result
        (for [row-n (range (count rows))
              col-n (range (count cols))]
          (if (or (= 0 row-n)
                  (= 0 col-n)
                  (= (dec (count rows)) row-n)
                  (= (dec (count cols)) col-n))
            0
            (let [row (nth rows row-n)
                  col (nth cols col-n)
                  tree-height (nth (nth rows row-n) col-n)
                  dists
                  (map
                   (fn [dir]
                     (let [path (cond
                                  (= :up dir)
                                  (reverse (subvec col 0 row-n))

                                  (= :down dir)
                                  (subvec col (inc row-n))

                                  (= :left dir)
                                  (reverse (subvec row 0 col-n))

                                  (= :right dir)
                                  (subvec row (inc col-n)))
                           count-til-blocked (count (take-while #(< % tree-height) path))]
                       (if (= count-til-blocked (count path))
                         count-til-blocked
                         (inc count-til-blocked))))
                   dirs)]
              (apply * dists))))]
    (apply max result)))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 8))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 8)))
