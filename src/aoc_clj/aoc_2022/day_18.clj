(ns aoc-clj.aoc-2022.day-18
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [aoc-clj.utils :as utils]))

(def sample-input
  "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #","))
       (map (fn [p] (map parse-long p)))
       set))

(defn get-adjacent-cubes
  [[x y z]]
  (set [[(inc x) y z]
        [(dec x) y z]
        [x (inc y) z]
        [x (dec y) z]
        [x y (inc z)]
        [x y (dec z)]]))

(defn count-adjacent-faces
  [cubes cube]
  (let [adjacents (get-adjacent-cubes cube)]
    (set/difference adjacents cubes)))

(defn part-1
  [input]
  (let [cubes (parse-input input)
        exposed (reduce #(assoc %1 %2 (count-adjacent-faces cubes %2)) {} cubes)
        exposed-per-cube (map count (vals exposed))]
    (apply + exposed-per-cube)))

(defn floodfill
  [{[x-low y-low z-low] :low-bound
    [x-high y-high z-high] :high-bound
    cubes :cubes
    :as structure}
   visited
   [x y z :as cube]]
  (if (or (< x x-low)
          (< y y-low)
          (< z z-low)
          (> x x-high)
          (> y y-high)
          (> z z-high))
    []
    (let [adj (get-adjacent-cubes cube)
          next-moves (set/difference adj cubes visited)]
      (if (empty? next-moves)
        [cube]
        (into #{} (mapcat #(floodfill structure (conj visited %) %) next-moves))))))

(defn bounds
  [cubes]
  (let [low-x (apply min (map first cubes))
        high-x (apply max (map first cubes))
        low-y (apply min (map second cubes))
        high-y (apply max (map second cubes))
        low-z (apply min (map #(nth % 2) cubes))
        high-z (apply max (map #(nth % 2) cubes))]
    [[low-x low-y low-z] [high-x high-y high-z]]))

(defn start-floodfill
  [structure point]
  (floodfill structure #{point} point))

(defn fill-voids
  [{:keys [cubes low-bound] :as structure}]
  (loop [current-cubes cubes
         n 0]
    (if (>= n 3)
      nil
      (let [exposed (reduce #(assoc %1 %2 (count-adjacent-faces current-cubes %2)) {} current-cubes)

            all-exposed (apply set/union (vals exposed))
            exposed-to-air (start-floodfill structure (map inc low-bound))
            not-exposed (set/difference all-exposed (set exposed-to-air))]
        (pprint not-exposed)
        (if (= 0 (count not-exposed))
          (let [exposed-per-cube (map count (vals exposed))]
            (apply + exposed-per-cube))
          (recur (set/union current-cubes not-exposed) (inc n)))))))

(defn part-2
  [input]
  (let [cubes (parse-input input)
        [low-bound high-bound] (bounds cubes)
        structure {:low-bound (map #(- % 2) low-bound)
                   :high-bound (map #(+ % 2) high-bound)
                   :cubes cubes}]
    #_(fill-voids structure)
    (start-floodfill structure (map inc high-bound))))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 18))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 18)))
