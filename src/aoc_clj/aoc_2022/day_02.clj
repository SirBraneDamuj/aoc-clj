(ns aoc-clj.aoc-2022.day-02
  (:require [clojure.string :as str]))

(def sample-input
  "A Y
B X
C Z")

(def throw-scores
  {\X 1
   \Y 2
   \Z 3})

;; A ROCK
;; B PAPER
;; C SCISSORS

;; X ROCK
;; Y PAPER
;; Z SCISSORS

(def round-scores
  {\X {\A 3
       \B 0
       \C 6}
   \Y {\A 6
       \B 3
       \C 0}
   \Z {\A 0
       \B 6
       \C 3}})

(defn score-round
  [round-str]
  (let [[you _ me] round-str
        score (+ (get throw-scores me) (get-in round-scores [me you]))
        _ (println round-str score)]
    score))

(defn part-1
  [input]
  (let [lines (str/split-lines input)
        scores (map score-round lines)]
    (apply + scores)))

(def wins
  {\A \Y
   \B \Z
   \C \X})

(def losses
  {\A \Z
   \B \X
   \C \Y})

(def draws
  {\A \X
   \B \Y
   \C \Z})

(def scores
  {\X 0
   \Y 3
   \Z 6})

(defn plan-round
  [round-str]
  (let [[you _ outcome] round-str
        throw (cond
                (= outcome \X)
                (get losses you)

                (= outcome \Y)
                (get draws you)

                (= outcome \Z)
                (get wins you))
        throw-score (get throw-scores throw)
        round-score (get scores outcome)]
    (+ throw-score round-score)))

(defn part-2
  [input]
  (let [lines (str/split-lines input)
        scores (map plan-round lines)]
    (apply + scores)))

(comment
  (require '[aoc-clj.core :as aoc])
  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 2))
  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 2)))
