(ns aoc-clj.aoc-2022.day-11
  (:require [clojure.string :as str]
            [aoc-clj.utils :as util]
            [clojure.pprint :refer [pprint]]))

(def sample-input
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(def cache
  (atom {}))

(defn parse-start-line
  [start-line]
  (let [[_ items-csv] (str/split start-line #": ")
        items (str/split items-csv #", ")]
    (mapv bigint items)))

(def op-fns
  {"+" +
   "*" *})

(defn parse-op-line
  [op-line]
  (let [[_ op-string] (str/split op-line #": ")
        [_ _ operand-1-str operator operand-2-str] (str/split op-string #" ")
        op-fn (op-fns operator)
        operands [operand-1-str operand-2-str]]
    (fn [old]
      (apply op-fn (mapv #(if (= "old" %) old (parse-long %)) operands)))))

(defn parse-test-line
  [test-line]
  (let [tokens (str/split test-line #" ")]
    (parse-long (last tokens))))

(defn parse-cond-line
  [cond-line]
  (-> cond-line
      (str/split #" ")
      last
      parse-long))

(defn parse-monkey
  [monkey-str]
  (let [[_ start-line op-line test-line true-line false-line] (str/split-lines monkey-str)]
    {:items (parse-start-line start-line)
     :op (parse-op-line op-line)
     :test (parse-test-line test-line)
     :true-target (parse-cond-line true-line)
     :false-target (parse-cond-line false-line)
     :inspections 0}))

(defn parse-input
  [input]
  (->> input
       util/split-newline-delim-line-groups
       (mapv parse-monkey)))

(defn process-item
  [{:keys [op test true-target false-target]} monkeys item]
  (let [new-item (mod (op item) 9699690) #_(quot (op item) 3)
        test-result (zero? (mod new-item test))
        target (if test-result true-target false-target)]
    (-> monkeys
        (update-in [target :items] conj new-item))))

(defn process-monkey
  [monkeys n]
  (let [{:keys [items] :as monkey} (nth monkeys n)]
    (->> items
         (reduce (partial process-item monkey) monkeys)
         (#(update-in % [n :inspections] + (count items)))
         (#(assoc-in % [n :items] [])))))

(defn process-round
  [monkeys x]
  (reduce process-monkey monkeys (range (count monkeys))))

;; 20
(defn part-1
  [input]
  (let [monkeys (parse-input input)
        result (reduce process-round monkeys (range 20))
        sorted (sort-by :inspections result)]
    (apply * (map :inspections (take-last 2 sorted)))))

(defn part-2
  [input]
  (let [monkeys (parse-input input)
        result (reduce process-round monkeys (range 10000))
        sorted (sort-by :inspections result)]
    (apply * (map :inspections (take-last 2 sorted)))))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 11))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 11)))
