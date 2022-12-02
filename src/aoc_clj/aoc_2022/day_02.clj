(ns aoc-clj.aoc-2022.day-02
  (:require [clojure.string :as str]))

(def throw->score
  {:rock     1
   :paper    2
   :scissors 3})

(def char->throw
  {\X :rock
   \A :rock
   \Y :paper
   \B :paper
   \Z :scissors
   \C :scissors})

(def round-scores
  {:rock     {:rock     3
              :paper    0
              :scissors 6}
   :paper    {:rock     6
              :paper    3
              :scissors 0}
   :scissors {:rock     0
              :paper    6
              :scissors 3}})

(defn score-round-of-throws
  [round-str]
  (let [[you me] (->> round-str
                      (filter #(not= % \ ))
                      (map char->throw))]
    (+ (throw->score me) (get-in round-scores [me you]))))

(defn part-1
  [input]
  (let [lines (str/split-lines input)
        scores (map score-round-of-throws lines)]
    (apply + scores)))

(def char->outcome
  {\X :lose
   \Y :draw
   \Z :win})

(def plan
  {:lose {:rock :scissors
          :paper :rock
          :scissors :paper}
   :draw {:rock :rock
          :paper :paper
          :scissors :scissors}
   :win  {:rock :paper
          :paper :scissors
          :scissors :rock}})

(def outcome->score
  {:lose 0
   :draw 3
   :win  6})

(defn score-round-of-throw-and-outcome
  [round-str]
  (let [you (-> round-str
                first
                char->throw)
        outcome (-> round-str
                    last
                    char->outcome)
        planned-throw (get-in plan [outcome you])]
    (+ (throw->score planned-throw) (outcome->score outcome))))

(defn part-2
  [input]
  (let [lines (str/split-lines input)
        scores (map score-round-of-throw-and-outcome lines)]
    (apply + scores)))

(def solution
  {:year 2022
   :day 2
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
