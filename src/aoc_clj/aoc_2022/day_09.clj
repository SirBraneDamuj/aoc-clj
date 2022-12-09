(ns aoc-clj.aoc-2022.day-09
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]
            [clojure.pprint :refer [pprint]]))

(def sample-input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def sample-input-2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

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


(defn move-toward-point
  [[x y] [target-x target-y]]
  (let [x-distance (- target-x x)
        y-distance (- target-y y)]
    (cond
      (and (zero? x-distance)
           (zero? y-distance))
      [x y]

      (zero? x-distance)
      (cond
        (or (zero? y-distance)
            (= 1 (abs y-distance)))
        [x y]

        :else
        [x ((if (pos? y-distance) inc dec) y)])

      (= 1 (abs x-distance))
      (cond
        (or (zero? y-distance)
            (= 1 (abs y-distance)))
        [x y]

        :else
        [((if (pos? x-distance) inc dec) x)
         ((if (pos? y-distance) inc dec) y)])

      :else
      (cond
        (zero? y-distance)
        [((if (pos? x-distance) inc dec) x) y]

        :else
        [((if (pos? x-distance) inc dec) x)
         ((if (pos? y-distance) inc dec) y)]))))

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

(defn part-1
  [input]
  (let [lines (str/split-lines input)
        motions (map parse-motion lines)
        {:keys [t-positions]} (reduce
                               perform-motion
                               {:t-positions #{[0 0]}
                                :knots [[0 0] [0 0]]}
                               motions)]
    (count t-positions)))

(defn part-2
  [input]
  (let [lines (str/split-lines input)
        motions (map parse-motion lines)
        knots (into [] (repeat 10 [0 0]))
        {:keys [t-positions]} (reduce
                               perform-motion
                               {:t-positions #{[0 0]}
                                :knots knots}
                               motions)]
    (count t-positions)))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 9))

  (part-2 sample-input-2)
  (part-2 (aoc/get-puzzle-input 2022 9)))
