(ns aoc-clj.aoc-2023.day-03
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]
            [clojure.pprint :refer [pprint]]))

(def sample-input
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def digits #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})

(defn line->symbols
  [n line]
  (->> line
       (map-indexed (fn [index c]
                      [index c]))
       (reduce (fn [symbols [pos c]]
                 (if (or (= \. c)
                         (digits c))
                   symbols
                   (conj symbols [{:row n
                                   :col pos} c])))
               [])))

(comment
  (line->symbols 3 ".....+.58."))

(defn line->numbers
  [n line]
  (let [{:keys [active-num nums]}
        (->> line
             (map-indexed (fn [index c]
                            [index c]))
             (reduce (fn [{:keys [active-num nums]} [pos c]]
                       (if active-num
                         (if-not (digits c)
                           {:active-num nil
                            :nums (conj nums active-num)}
                           {:active-num (conj active-num [{:row n
                                                           :col pos} c])
                            :nums nums})
                         (if (digits c)
                           {:active-num [[{:row n
                                           :col pos} c]]
                            :nums nums}
                           {:active-num active-num
                            :nums nums})))
                     {:active-num nil
                      :nums []}))]
    (if active-num
      (conj nums active-num)
      nums)))

(comment
  (line->numbers 3 "..35..633."))

(defn index-positions
  [symbols]
  (into {}
        (map (fn [[pos c]]
               [pos c])
             symbols)))

(defn index-numbers
  [numbers]
  (into {}
        (mapcat (fn [number]
                  (map (fn [[pos _]]
                         [pos number])
                       number))
                numbers)))

(comment
  (index-numbers [(line->numbers 2 "..35..633.")]))

(defn pos-adjs
  [{:keys [row col]}]
  [{:row (dec row)
    :col (dec col)}
   {:row (dec row)
    :col col}
   {:row (dec row)
    :col (inc col)}
   {:row row
    :col (dec col)}
   {:row row
    :col col}
   {:row row
    :col (inc col)}
   {:row (inc row)
    :col (dec col)}
   {:row (inc row)
    :col col}
   {:row (inc row)
    :col (inc col)}])

(defn check-digit-adjacency
  [[pos _] idx]
  (let [adjs (pos-adjs pos)]
    (boolean
     (some idx adjs))))

(defn check-num-adjacency
  [num idx]
  (boolean
   (some #(check-digit-adjacency % idx) num)))

(defn num->int
  [num]
  (pprint num)
  (->> num
       (map second)
       (apply str)
       Integer/parseInt))

(defn part-1
  [input]
  (let [lines (str/split input #"\n")
        symbols (apply concat (map-indexed line->symbols lines))
        sym-index (index-positions (filter seq symbols))
        nums (apply concat (map-indexed line->numbers lines))
        valid-nums (filter #(check-num-adjacency % sym-index) nums)
        valids (map num->int valid-nums)]
    (reduce + valids)))

(def sample-input-2
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn get-gear-ratio
  [[pos _] num-index]
  (pprint pos)
  (let [gear-adjs (pos-adjs pos)
        adj-nums (map num-index gear-adjs)
        adj-nums-uniq (set (filter some? adj-nums))]
    (pprint adj-nums-uniq)
    (if (= 2 (count adj-nums-uniq))
      (apply * (map num->int adj-nums-uniq))
      0)))

(defn part-2
  [input]
  (let [lines (str/split input #"\n")
        symbols (apply concat (map-indexed line->symbols lines))
        gears (filter (fn [[_ c]] (= \* c)) symbols)
        nums (apply concat (map-indexed line->numbers lines))
        num-index (index-numbers nums)
        gear-ratios (map #(get-gear-ratio % num-index) gears)]
    (reduce + gear-ratios)))

(def solution
  {:year 2023
   :day 3
   :part-1 part-1
   :part-2 part-2})

(comment
  (part-1 sample-input)
  (part-2 sample-input-2)

  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
