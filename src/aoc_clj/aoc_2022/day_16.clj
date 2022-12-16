(ns aoc-clj.aoc-2022.day-16
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [aoc-clj.utils :as utils :refer [ppeek]]
            [clojure.pprint :refer [pprint]]))

(def sample-input
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(defn parse-line
  [line]
  (let [[valve-str tunnels-str] (str/split line #"; ")
        [_ valve-name _ _ rate-str] (str/split valve-str #" ")
        [_ rate-val-str] (str/split rate-str #"=")
        [_ tunnels-list] (str/split tunnels-str #"( valves )|( valve )")
        tunnels-names (str/split tunnels-list #", ")]
    {:valve valve-name
     :rate (parse-long rate-val-str)
     :tunnels tunnels-names
     :status :closed}))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map parse-line)
       (reduce #(assoc %1 (:valve %2) %2) {})))

(def cache (atom {}))

(defn max-value-of-move
  [valves position moves-remaining print-progress?]
  (let [{:keys [rate status tunnels]} (get valves position)
        open-valves (filter #(= :open (:status %)) (vals valves))
        open-valve-names (map :valve open-valves)
        accrued-value (or (apply + (map :rate open-valves)) 0)
        cache-key [position moves-remaining (str/join (sort open-valve-names)) accrued-value]]
    (if-let [cached (get @cache cache-key)]
      cached
      (let [result (if (<= moves-remaining 0)
                     accrued-value
                     (let [possible-moves (if (or (= rate 0)
                                                  (= :open status))
                                            tunnels
                                            (conj tunnels position))
                           move-values (for [move possible-moves]
                                         (let [new-valves (if (= position move)
                                                            (assoc-in valves [position :status] :open)
                                                            valves)]

                                           (when print-progress?
                                             (pprint ["PROGRESS: FINISHED FIRST MOVE" move]))
                                           (max-value-of-move new-valves move (dec moves-remaining) false)))]
                       (+ accrued-value (apply max move-values))))]
        (swap! cache assoc cache-key result)
        result))))

(defn part-1
  [input]
  (reset! cache {})
  (let [valves (parse-input input)]
    (max-value-of-move valves "AA" 29 true)))

(defn part-2
  [input])

(comment
  (require '[aoc-clj.core :as aoc])

  (let [valves (parse-input sample-input)]
    (map :valve (filter #(= :closed (:status %)) (vals valves))))

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 16))

  (+ 1 2)

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 16)))