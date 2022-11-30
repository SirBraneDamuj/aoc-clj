(ns aoc-clj.aoc-2021.day-02
  (:require [clojure.string :as str]))

(defn handle-naive-command
  [sub command]
  (let [[command arg-string] (str/split command #" ")
        arg (parse-long arg-string)]
    (cond
      (= "forward" command)
      (update sub :pos #(+ % arg))

      (= "up" command)
      (update sub :depth #(- % arg))

      (= "down" command)
      (update sub :depth #(+ % arg)))))

(defn part-1
  [input]
  (let [lines (str/split-lines input)
        {:keys [depth pos]} (reduce
                             handle-naive-command
                             {:depth 0 :pos 0}
                             lines)]
    (* depth pos)))

(defn handle-aim-command
  [{:keys [aim] :as sub} command]
  (let [[command arg-string] (str/split command #" ")
        arg (parse-long arg-string)]
    (cond
      (= "forward" command)
      (-> sub
          (update :pos #(+ % arg))
          (update :depth #(+ % (* aim arg))))

      (= "up" command)
      (update sub :aim #(- % arg))

      (= "down" command)
      (update sub :aim #(+ % arg)))))

(defn part-2
  [input]
  (let [lines (str/split-lines input)
        {:keys [depth pos]} (reduce
                             handle-aim-command
                             {:depth 0 :pos 0 :aim 0}
                             lines)]
    (* depth pos)))

(def solution
  {:year 2021
   :day 2
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
