(ns aoc-clj.aoc-2022.day-08
  (:require [clojure.string :as str]))

(defn coordinates
  [num-rows num-cols]
  (for [row-n (range num-rows)
        col-n (range num-cols)]
    [row-n col-n]))

(defn parse-rows
  [input]
  (let [lines (str/split-lines input)]
    (mapv
     #(mapv (comp parse-long str) %)
     lines)))

(defn parse-cols
  [input]
  (let [lines (str/split-lines input)]
    (into []
          (for [x (range (count (first lines)))]
            (mapv #(parse-long (str (nth % x))) lines)))))

(defn parse-grid
  [input]
  (let [rows (parse-rows input)
        cols (parse-cols input)
        coords (coordinates (count rows) (count cols))]
    {:rows rows
     :cols cols
     :coords coords}))

(def dirs
  [[:up    (fn [_ col row-n _]
             (reverse (subvec col 0 row-n)))]
   [:down  (fn [_ col row-n _]
             (subvec col (inc row-n)))]
   [:left  (fn [row _ _ col-n]
             (reverse (subvec row 0 col-n)))]
   [:right (fn [row _ _ col-n]
             (subvec row (inc col-n)))]])


(defn edge?
  [rows cols row-n col-n]
  (or (= 0 row-n)
      (= 0 col-n)
      (= (dec (count rows)) row-n)
      (= (dec (count cols)) col-n)))

(defn tree-visible?
  [row col row-n col-n]
  (let [height (nth row col-n)]
    (some
     (fn [[_ path-fn]]
       (let [path (path-fn row col row-n col-n)]
         (every? #(< % height) path)))
     dirs)))

(defn part-1
  [input]
  (let [{:keys [rows cols coords]} (parse-grid input)]
    (->> coords
         (filter
          (fn [[row-n col-n]]
            (if (edge? rows cols row-n col-n)
              true
              (let [row (nth rows row-n)
                    col (nth cols col-n)]
                (tree-visible? row col row-n col-n)))))
         count)))

(defn scenic-score
  [row col row-n col-n]
  (let [height (nth row col-n)]
    (->> dirs
         (map (fn [[_ path-fn]]
                (let [path (path-fn row col row-n col-n)
                      count-til-blocked (count (take-while #(< % height) path))]
                  (if (= count-til-blocked (count path))
                    count-til-blocked
                    (inc count-til-blocked)))))
         (reduce *))))

(defn part-2
  [input]
  (let [{:keys [rows cols coords]} (parse-grid input)]
    (->> coords
         (map
          (fn [[row-n col-n]]
            (if (edge? rows cols row-n col-n)
              0
              (let [row (nth rows row-n)
                    col (nth cols col-n)]
                (scenic-score row col row-n col-n)))))
         (apply max))))

(def solution
  {:year 2022
   :day 8
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
