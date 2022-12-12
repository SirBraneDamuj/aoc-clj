(ns aoc-clj.aoc-2022.day-12
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(def ^:private inf Long/MAX_VALUE)

(def sample-input
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse-square
  [c]
  (cond
    (= \S c)
    :start

    (= \E c)
    :end

    :else
    (- (int c) 97)))

(defn parse-line
  [s]
  (mapv parse-square s))

(defn parse-grid
  [input]
  (->> input
       str/split-lines
       (mapv parse-line)))

(def cache
  (atom {}))

(defn outside-grid
  [grid [x y]]
  (or (>= x (count (first grid)))
      (< x 0)
      (>= y (count grid))
      (< y 0)))

(defn get-cell
  [grid [x y]]
  (nth (nth grid y) x))

(defn cell->elevation
  [cell]
  (cond
    (= :start cell)
    0
    (= :end cell)
    25
    :else
    cell))
;; this one is for part 2
#_(defn valid-move
    [grid start end]
    (let [start-elevation (cell->elevation (get-cell grid start))
          end-elevation (cell->elevation (get-cell grid end))
          difference (- end-elevation start-elevation)]
      (>= difference -1)))
;; this one is for part 1. figure out how to use the correct one for the correct part
(defn valid-move
  [grid start end]
  (let [start-elevation (cell->elevation (get-cell grid start))
        end-elevation (cell->elevation (get-cell grid end))
        difference (- end-elevation start-elevation)]
    (<= difference 1)))

(defn available-moves
  [grid [x y]]
  (let [left [(dec x) y]
        right [(inc x) y]
        up [x (dec y)]
        down [x (inc y)]]
    (->> [left right up down]
         (filter (complement (partial outside-grid grid)))
         (filter (partial valid-move grid [x y])))))


(defn coordinates
  [num-rows num-cols]
  (for [row-n (range num-rows)
        col-n (range num-cols)]
    [col-n row-n]))

(defn edges
  [grid]
  (let [points (coordinates (count grid) (count (first grid)))]
    (reduce
     (fn [acc p]
       (if-let [moves (seq (available-moves grid p))]
         (assoc acc p (zipmap moves (repeat 1)))
         acc))
     {}
     points)))

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
     (fn [c nbr nbr-cost]
       (if (unvisited nbr)
         (update-in c [nbr] min (+ curr-cost nbr-cost))
         c))
     costs
     (get g curr))))
;; http://www.loganlinn.com/blog/2013/04/22/dijkstras-algorithm-in-clojure/
;; not ashamed. I'm not writing dijkstra for the thousandth time
(defn dijkstra
  ([g src]
   (dijkstra g src nil))
  ([g src dst]
   (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
          curr src
          unvisited (disj (apply hash-set (keys g)) src)]
     (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr)))
       costs

       :else
       (let [next-costs (update-costs g costs unvisited curr)
             next-node (apply min-key next-costs unvisited)]
         (recur next-costs next-node (disj unvisited next-node)))))))

(defn part-1
  [input]
  (let [grid (parse-grid input)
        graph (edges grid)
        start (->> (keys graph)
                   (filter #(= :start (get-cell grid %)))
                   first)
        end (->> (keys graph)
                 (filter #(= :end (get-cell grid %)))
                 first)]
    (dijkstra graph start end)))

(defn part-2
  [input]
  (let [grid (parse-grid input)
        graph (edges grid)
        starts (->> (keys graph)
                    (filter #(or (= 0 (get-cell grid %))
                                 (= :start (get-cell grid %)))))
        _ (pprint starts)
        end (->> (keys graph)
                 (filter #(= :end (get-cell grid %)))
                 first)
        end-costs (dijkstra graph end)
        start-costs (map end-costs starts)]
    (apply min start-costs)))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 12))

  (+ 1 2)

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 12))
  (seq #{1}))
