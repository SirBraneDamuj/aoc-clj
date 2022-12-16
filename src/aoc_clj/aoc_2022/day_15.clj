(ns aoc-clj.aoc-2022.day-15
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.math :as math]
            [aoc-clj.utils :refer [ppeek] :as utils]))

(def sample-input
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn parse-coords
  [x-str y-str]
  (let [[_ x-num-str] (str/split x-str #"=")
        [_ y-num-str] (str/split y-str #"=")]
    [(parse-long (subs x-num-str 0 (- (count x-num-str) 1)))
     (parse-long y-num-str)]))

(defn parse-sensor
  [sensor-str]
  (let [[_ _ x-str y-str] (str/split sensor-str #" ")]
    (parse-coords x-str y-str)))

(defn parse-beacon
  [beacon-str]
  (let [[_ _ _ _ x-str y-str] (str/split beacon-str #" ")]
    (parse-coords x-str y-str)))

(defn parse-line
  [line]
  (let [[sensor-str beacon-str] (str/split line #": ")]
    [(parse-sensor sensor-str) (parse-beacon beacon-str)]))

(defn parse-input
  [input]
  (map parse-line (str/split-lines input)))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn range-at-y
  [[sensor beacon] c]
  (let [d (manhattan-distance sensor beacon)
        [a b] sensor
        k (- d (abs (- b c)))
        bound-1 (+ k a)
        bound-2 (- a k)]
    [bound-2 bound-1]
    #_(if (> bound-2 bound-1)
        [bound-1 bound-2]
        [bound-2 bound-1])))

(defn part-1
  [input target-y]
  (let [sensors (parse-input input)
        beacon-set (into #{} (map second sensors))
        ranges (map #(range-at-y % target-y) sensors)
        range-set (reduce #(into %1 %2) #{} (map (fn [[lower upper]] (range lower (inc upper))) ranges))
        beacons-in-ranges (count
                           (filter
                            (fn [[beacon-x beacon-y]]
                              (some
                               (fn [[lower upper _]]
                                 (and (= target-y beacon-y)
                                      (>= beacon-x lower)
                                      (<= beacon-x upper)))
                               ranges))
                            beacon-set))]
    (- (count range-set) beacons-in-ranges)))

(defn complete-range?
  [ranges]
  (loop [sorted-ranges (sort-by first (filter #(seq (range (first %) (second %))) ranges))]
    (if (= 1 (count sorted-ranges))
      true
      (let [[first-min first-max] (first sorted-ranges)
            rest-ranges (rest sorted-ranges)
            next-range (some
                        (fn [r] (when (<= (first r) (inc first-max)) r))
                        rest-ranges)]
        (if next-range
          (recur (into [[first-min (max first-max (second next-range))]]
                       (filter #(not= next-range %) rest-ranges)))
          false)))))

(defn part-2
  [input target-high]
  (doseq [y (range target-high)]
    (when (zero? (mod y 10000)) (pprint y))
    (let [sensors (parse-input input)
          ranges (map #(range-at-y % y) sensors)
          clamped-ranges (map (fn [[lower upper]]
                                [(max lower 0) (min upper target-high)])
                              ranges)]
      (when-not (complete-range? clamped-ranges)
        (pprint ["NOT COMPLETE RANGE AT Y:" y])
        (doseq [x (range target-high)]
          (let [contained? (some (fn [[lower upper]]
                                   (and (>= x lower)
                                        (<= x upper)))
                                 clamped-ranges)]
            (when-not contained? (pprint ["FOUND IT" x y]))))))))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input 10)
  (part-1 (aoc/get-puzzle-input 2022 15) 2000000)

  (part-2 sample-input 20)
  (part-2 (aoc/get-puzzle-input 2022 15) 4000000))
