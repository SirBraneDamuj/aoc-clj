(ns aoc-clj.aoc-2022.day-05
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]))

(defn cross-sections->nth-stack
  [cross-sections stack-n]
  (into [] (->> cross-sections
                (map #(nth % stack-n))
                (take-while #(not= % \space)))))

(defn parse-diagram
  [s]
  (let [cross-sections (->> (str/split-lines s)
                            reverse
                            rest
                            (mapv #(->> %
                                        (drop 1)
                                        (take-nth 4))))
        num-stacks (count (first cross-sections))]
    (mapv #(cross-sections->nth-stack cross-sections %)
          (range num-stacks))))

(defn parse-instruction
  [s]
  (let [[_ n-str _ from-str _ to-str] (str/split s #" ")
        [n from to] (mapv parse-long [n-str from-str to-str])]
    [n (dec from) (dec to)]))

(defn parse-instructions
  [s]
  (let [lines (str/split-lines s)]
    (mapv parse-instruction lines)))

(defn move-crate
  [[from to] _]
  (let [crate (last from)
        new-from (vec (drop-last from))
        new-to (conj to crate)]
    [new-from new-to]))

(defn perform-instruction-9000
  [stacks [n from to]]
  (let [[new-from new-to] (reduce
                           move-crate
                           [(nth stacks from) (nth stacks to)]
                           (range n))]
    (-> stacks
        (assoc from new-from)
        (assoc to new-to))))

(defn part-1
  [input]
  (let [[diagram instr-str] (util/split-newline-delim-line-groups input)
        stacks (parse-diagram diagram)
        instructions (parse-instructions instr-str)
        result (reduce perform-instruction-9000 stacks instructions)]
    (str/join (mapv last result))))

(defn move-crates
  [n from to]
  (let [crates (take-last n from)
        new-to (concat to crates)
        new-from (drop-last n from)]
    [new-from new-to]))

(defn perform-instruction-9001
  [stacks [n from to]]
  (let [[new-from new-to] (move-crates
                           n
                           (nth stacks from)
                           (nth stacks to))]
    (-> stacks
        (assoc from new-from)
        (assoc to new-to))))

(defn part-2
  [input]
  (let [[diagram instr-str] (util/split-newline-delim-line-groups input)
        stacks (parse-diagram diagram)
        instructions (parse-instructions instr-str)
        result (reduce perform-instruction-9001 stacks instructions)]
    (str/join (mapv last result))))

(def solution
  {:year 2022
   :day 5
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
