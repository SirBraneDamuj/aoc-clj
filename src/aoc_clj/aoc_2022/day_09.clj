(ns aoc-clj.aoc-2022.day-09
  (:require [clojure.string :as str]))

(defn parse-motion
  [s]
  (let [[dir count] (str/split s #" ")]
    [dir (parse-long count)]))

(defn move-point
  [[x y] dir]
  (cond
    (= "U" dir)
    [x (dec y)]

    (= "D" dir)
    [x (inc y)]

    (= "L" dir)
    [(dec x) y]

    (= "R" dir)
    [(inc x) y]))

(defn move-component-if-needed
  [n distance]
  (cond
    (zero? distance)
    n

    (neg? distance)
    (dec n)

    (pos? distance)
    (inc n)))

(defn move-toward-point
  [[x y] [target-x target-y]]
  (let [x-distance (- target-x x)
        y-distance (- target-y y)]
    (if (or (> (abs x-distance) 1)
            (> (abs y-distance) 1))
      [(move-component-if-needed x x-distance)
       (move-component-if-needed y y-distance)]
      [x y])))

(defn perform-step
  [dir {:keys [knots t-positions]} _]
  (let [new-knots (reduce
                   (fn [new-knots t-pos]
                     (let [h-pos (last new-knots)]
                       (conj new-knots (move-toward-point t-pos h-pos))))
                   [(move-point (first knots) dir)]
                   (drop 1 knots))]
    {:knots new-knots
     :t-positions (conj t-positions (last new-knots))}))

(defn perform-motion
  [{:keys [knots t-positions]} [dir distance]]
  (reduce
   (partial perform-step dir)
   {:knots knots
    :t-positions t-positions}
   (range distance)))

(defn trace-rope
  [input size]
  (let [lines (str/split-lines input)
        motions (map parse-motion lines)
        knots (repeat size [0 0])
        {:keys [t-positions]} (reduce
                               perform-motion
                               {:t-positions #{[0 0]}
                                :knots knots}
                               motions)]
    (count t-positions)))

(defn part-1
  [input]
  (trace-rope input 2))

(defn part-2
  [input]
  (trace-rope input 10))

(def solution
  {:year 2022
   :day 9
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
