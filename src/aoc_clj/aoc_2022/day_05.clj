(ns aoc-clj.aoc-2022.day-05
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

;;     [D]
;; [N]     [C]
;; [Z] [M] [P]
;; 1   2   3

;; move 1 from 2 to 1
;; move 3 from 1 to 3
;; move 2 from 2 to 1
;; move 1 from 1 to 2

(def sample-input
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-diagram
  [s]
  (let [lines (str/split-lines s)
        across (mapv #(->> %
                           (drop 1)
                           (take-nth 4)) lines)
        stacks (->> across
                    last
                    (mapv (comp dec parse-long str))
                    vec)
        stack-lines (reverse (drop-last 1 across))]
    (mapv (fn [stack-n]
            (->> stack-lines
                 (mapv #(nth % stack-n))
                 (take-while #(not= % \space))
                 vec))
          stacks)))

(defn parse-instructions
  [s]
  (let [lines (str/split-lines s)]
    (mapv
     (fn [line]
       (let [[_ n _ from _ to] (str/split line #" ")]
         (mapv parse-long [n from to])))
     lines)))

(defn perform-instruction
  [from to n]
  (if (zero? n)
    [from to]
    (let [crate (last from)
          new-to (conj to crate)
          new-from (vec (drop-last from))]
      (perform-instruction new-from new-to (dec n)))))

(defn perform-instructions
  [stacks instructions]
  (if-not (seq instructions)
    stacks
    (let [[n from to] (first instructions)
          [new-from new-to] (perform-instruction
                             (nth stacks (dec from))
                             (nth stacks (dec to))
                             n)
          new-stacks (-> stacks
                         (assoc (dec from) new-from)
                         (assoc (dec to) new-to))]
      (perform-instructions new-stacks (rest instructions)))))

(defn part-1
  [input]
  (let [[diagram instr-str] (str/split input #"\n\n")
        stacks (parse-diagram diagram)
        instructions (parse-instructions instr-str)
        result (perform-instructions stacks instructions)]
    (str/join (mapv last result))))

(defn perform-instruction-9001
  [from to n]
  (let [crates (take-last n from)
        new-to (concat to crates)
        new-from (drop-last n from)]
    [new-from new-to]))

(defn perform-instructions-9001
  [stacks instructions]
  (if-not (seq instructions)
    stacks
    (let [[n from to] (first instructions)
          [new-from new-to] (perform-instruction-9001
                             (nth stacks (dec from))
                             (nth stacks (dec to))
                             n)
          new-stacks (-> stacks
                         (assoc (dec from) new-from)
                         (assoc (dec to) new-to))]
      (perform-instructions-9001 new-stacks (rest instructions)))))

(defn part-2
  [input]
  (let [[diagram instr-str] (str/split input #"\n\n")
        stacks (parse-diagram diagram)
        instructions (parse-instructions instr-str)
        result (perform-instructions-9001 stacks instructions)]
    (str/join (mapv last result))))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 5))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 5)))
