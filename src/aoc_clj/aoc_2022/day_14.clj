(ns aoc-clj.aoc-2022.day-14
  (:require [clojure.string :as str]
            [aoc-clj.utils :as utils :refer [ppeek]]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

(def sample-input
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn points-along-line
  [[x1 y1] [x2 y2]]
  (set
   (if (= x1 x2)
     (if (< y1 y2)
       (map (fn [y] [x1 y]) (range y1 (inc y2)))
       (map (fn [y] [x1 y]) (range y2 (inc y1))))
     (if (< x1 x2)
       (map (fn [x] [x y1]) (range x1 (inc x2)))
       (map (fn [x] [x y1]) (range x2 (inc x1)))))))

(defn parse-point
  [point-str]
  (let [components (str/split point-str #",")]
    (map parse-long components)))

(defn parse-line
  [line]
  (let [point-strs (str/split line #" -> ")
        vertices (map parse-point point-strs)
        pairs (partition 2 1 vertices)]
    (reduce (fn [acc [p1 p2]]
              (set/union acc (points-along-line p1 p2)))
            #{}
            pairs)))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map parse-line)
       (apply set/union)
       (reduce #(assoc %1 %2 :rock) {})))

(defn next-sand-fall
  [grid max-y]
  (loop [[sand-x sand-y] [500 0]]
    (if (> sand-y max-y)
      nil
      (let [current [sand-x sand-y]
            down [sand-x (inc sand-y)]
            down-left [(dec sand-x) (inc sand-y)]
            down-right [(inc sand-x) (inc sand-y)]
            options [down down-left down-right]]
        (if (every? #(get grid %) options)
          current
          (recur (some #(if (get grid %) nil %) options)))))))



(defn print-grid
  [grid]
  (let [min-x (apply min (map first (keys grid)))
        max-x (apply max (map first (keys grid)))
        min-y (apply min (map second (keys grid)))
        max-y (apply max (map second (keys grid)))]
    (doseq [y (range 0 (inc max-y))]
      (let [row (map #(let [cell (get grid [% y])]
                        (cond
                          (= [500 0] [% y])
                          "X"
                          (= :rock cell)
                          "#"
                          (= :sand cell)
                          "o"
                          :else
                          ".")) (range min-x (inc max-x)))]
        (println (str/join row))))))

(defn part-1-real
  [input]
  (let [start-grid (parse-input input)
        max-y (apply max (map second (keys start-grid)))]
    (print-grid start-grid)
    (loop [grid start-grid]
      (let [next-sand (next-sand-fall grid max-y)]
        (if next-sand
          (recur (assoc grid next-sand :sand))
          grid)))))

(defn part-1
  [input]
  (let [result (part-1-real input)]
    (println "FOOBARFOOBARFOOBARFOOBARFOOBARFOOBARFOOBAR")
    (print-grid result)
    (->> result
         vals
         (filter #(= :sand %))
         count)))

(defn next-sand-fall-2
  [grid max-y]
  (loop [[sand-x sand-y] [500 0]]
    (if (= max-y (inc sand-y))
      [sand-x sand-y]
      (let [current [sand-x sand-y]
            down [sand-x (inc sand-y)]
            down-left [(dec sand-x) (inc sand-y)]
            down-right [(inc sand-x) (inc sand-y)]
            options [down down-left down-right]]
        (if (every? #(get grid %) options)
          current
          (recur (some #(when-not (get grid %) %) options)))))))

(defn part-2-real
  [input]
  (let [start-grid (parse-input input)
        max-y (+ 2 (apply max (map second (keys start-grid))))]
    (print-grid start-grid)
    (loop [grid start-grid]
      (let [next-sand (next-sand-fall-2 grid max-y)]
        (if (and next-sand
                 (not= [500 0] next-sand))
          (recur (assoc grid next-sand :sand))
          grid)))))

(defn part-2
  [input]
  (let [result (part-2-real input)]
    (println "FOOBARFOOBARFOOBARFOOBARFOOBARFOOBARFOOBAR")
    (print-grid result)
    (->> result
         vals
         (filter #(= :sand %))
         count)))

(comment
  (require '[aoc-clj.core :as aoc])
  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 14))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 14)))
