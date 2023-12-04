(ns aoc-clj.aoc-2023.day-02
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]))

(defn parse-group
  [group-str]
  (let [[num-str color-str] (str/split group-str #" ")]
    [color-str (Integer/parseInt num-str)]))

(defn parse-set
  [set-str]
  (let [group-strs (str/split set-str #", ")
        groups (map parse-group group-strs)]
    (into {} groups)))

(defn parse-game
  [line]
  (let [[game-id-str cubes-str] (str/split line #": ")
        game-id (-> game-id-str (str/split #" ") second Integer/parseInt)
        set-strs (str/split cubes-str #"; ")
        sets (map parse-set set-strs)]
    [game-id sets]))

(def rules
  [["red" 12]
   ["green" 13]
   ["blue" 14]])

(defn set-is-valid?
  [set]
  (boolean
   (some (fn [[color n]]
           (and
            (get set color)
            (<= (get set color) n)))
         rules)))

(defn game-is-valid?
  [[_ sets]]
  (boolean
   (some set-is-valid? sets)))

(defn part-1
  [input]
  (let [lines (str/split input #"\n")
        games (map parse-game lines)
        valid-games (filter game-is-valid? games)
        valid-game-ids (map first valid-games)]
    (println valid-games)
    (reduce + valid-game-ids)))

(defn minify-game
  [[_ sets]]
  (reduce (fn [acc set]
            (let [max-fn #(max (get acc %)
                               (get set % 0))]
              {"red" (max-fn "red")
               "green" (max-fn "green")
               "blue" (max-fn "blue")}))
          {"red" 0
           "green" 0
           "blue" 0}
          sets))

(defn part-2
  [input]
  (let [lines (str/split input #"\n")
        games (map parse-game lines)
        minified-games (map minify-game games)
        powers (map #(* (get % "red") (get % "green") (get % "blue")) minified-games)]
    (reduce + powers)))

(def solution
  {:year 2023
   :day 2
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
