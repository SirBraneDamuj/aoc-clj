(ns aoc-clj.aoc-2022.day-07
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def sample-input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn parse-cd
  [s]
  (let [[_ _ dir-name] (str/split s #" ")]
    dir-name))

(defn parse-ls
  [lines]
  (for [line (drop 1 lines)]
    (let [[size-or-dir name] (str/split line #" ")]
      (if (= size-or-dir "dir")
        {:name name
         :size 0}
        {:name name
         :size (parse-long size-or-dir)}))))

(defn partition-dirs
  [cmds]
  (reduce (fn [{:keys [depth lines dirs] :as acc} line]
            (cond
              (= "$ cd .." line)
              (let [new-depth (dec depth)
                    new-lines (if (zero? new-depth)
                                []
                                (conj lines line))
                    new-dirs (if (zero? new-depth)
                               (conj dirs lines)
                               dirs)]
                {:depth new-depth
                 :lines new-lines
                 :dirs new-dirs})

              (str/starts-with? line "$ cd")
              (-> acc
                  (update :depth inc)
                  (update :lines conj line))

              :else
              (update acc :lines conj line)))
          {:depth 0
           :lines []
           :dirs []}
          cmds))

(defn process-commands
  [cmds]
  (let [[_ _ dir-name] (str/split (first cmds) #" ")
        [ls-cmds cd-cmds] (split-with #(not (str/starts-with? % "$ cd")) (drop 1 cmds))
        contents (parse-ls ls-cmds)
        contents-size (reduce #(+ %1 (:size %2)) 0 contents)
        {:keys [dirs]} (partition-dirs cd-cmds)
        dirs-sizes (for [dir dirs] (process-commands dir))
        total-size (apply + contents-size (map :total-size dirs-sizes))]
    {:name dir-name
     :total-size total-size
     :children dirs-sizes}))

(defn sum-small-dirs
  [{:keys [name total-size children]}]
  (let [me (if (< total-size 100000)
             total-size
             0)
        children-sum (reduce + 0 (map sum-small-dirs children))]
    (+ me children-sum)))

(defn part-1
  [input]
  (let [cmds (conj (into [] (str/split-lines input))
                   "$ cd .."
                   "$ cd ..")
        tree (process-commands cmds)]
    (sum-small-dirs tree)))

(defn find-dirs-greater-than-size
  [{:keys [name total-size children]} size]
  (let [children-candidates (mapcat #(find-dirs-greater-than-size % size) children)]
    (if (> total-size size)
      (conj children-candidates total-size)
      children-candidates)))

(defn part-2
  [input]
  (let [cmds (conj (into [] (str/split-lines input))
                   "$ cd .."
                   "$ cd ..")
        {:keys [total-size] :as tree} (process-commands cmds)
        free-space (- 70000000 total-size)
        needed-space (- 30000000 free-space)
        candidates (find-dirs-greater-than-size tree needed-space)]
    (pprint needed-space)
    (apply min candidates)))

(comment
  (require '[aoc-clj.core :as aoc])

  (part-1 sample-input)
  (part-1 (aoc/get-puzzle-input 2022 7))

  (part-2 sample-input)
  (part-2 (aoc/get-puzzle-input 2022 7))

  (let [m {:foo [:bar :baz]}]
    (update m :foo conj :raggle)))
