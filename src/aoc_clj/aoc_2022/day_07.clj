(ns aoc-clj.aoc-2022.day-07
  (:require [clojure.string :as str]))

(defn parse-ls
  [lines]
  (for [line (drop 1 lines)]
    (let [[size-or-dir name] (str/split line #" ")]
      (if (= size-or-dir "dir")
        {:name name
         :size 0}
        {:name name
         :size (parse-long size-or-dir)}))))

(defn process-line
  [{:keys [depth lines dirs] :as acc} line]
  (cond
    (= "$ cd .." line)
    (let [new-depth (dec depth)
          ;; if the depth is balanced (0), begin a new partition.
          ;; empty out the line buffer and add it to the list of partitions
          ;; otherwise, this cd .. command is part of a child partition
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

(defn partition-dirs
  [cmds]
  (reduce process-line
          {:depth 0
           :lines []
           :dirs []}
          cmds))

(defn process-dir-partition
  "Nodes are never revisited, so we can treat blocks of lines as partitions
   defining the shape of that particular branch
   the simplest partition is a directory with no child dirs:
   cd a
   ls
   123 b.txt
   456 c.jpg
   cd ..
   
   a more complicated partition might contain several partitions within it:
   cd a
   ls
   dir b
   dir c
   123 d.txt
   cd b
   ls
   456 e.jpg
   cd ..
   cd c
   ls
   777 f.png
   cd ..
   cd ..
   
   this partition represents the entire contents of a. the a dir will never be revisited.
   the input represents a single large partition for the / directory
   we can then recursively process each of the smaller partitions within / to construct the tree"
  [cmds]
  (let [[_ _ dir-name] (str/split (first cmds) #" ")
        [ls-cmds cd-cmds] (split-with #(not (str/starts-with? % "$ cd")) (drop 1 cmds))
        contents (parse-ls ls-cmds)
        contents-size (reduce #(+ %1 (:size %2)) 0 contents)
        {:keys [dirs]} (partition-dirs cd-cmds)
        dirs-sizes (for [dir dirs] (process-dir-partition dir))
        total-size (apply + contents-size (map :total-size dirs-sizes))]
    {:name dir-name
     :total-size total-size
     :children dirs-sizes}))

(defn prepare-input
  "Appends extra cd .. commands to the end to balance the tree traversal.
   if the traversal goes down more than it goes up, we just add a few extra
   ups to the end to facilitate my parsing strategy"
  [input]
  (let [lines (str/split-lines input)
        [_ cd-cmds] (split-with #(not (str/starts-with? % "$ cd")) (drop 1 lines))
        {:keys [depth]} (partition-dirs cd-cmds)]
    (concat lines (repeat depth "$ cd .."))))

(defn sum-small-dirs
  [{:keys [total-size children]}]
  (let [me (if (< total-size 100000)
             total-size
             0)
        children-sum (reduce + 0 (map sum-small-dirs children))]
    (+ me children-sum)))

(defn part-1
  [input]
  (let [cmds (prepare-input input)
        tree (process-dir-partition cmds)]
    (sum-small-dirs tree)))

(defn find-dirs-greater-than-size
  [{:keys [total-size children]} size]
  (let [children-candidates (mapcat #(find-dirs-greater-than-size % size) children)]
    (if (> total-size size)
      (conj children-candidates total-size)
      children-candidates)))

(defn part-2
  [input]
  (let [cmds (prepare-input input)
        {:keys [total-size] :as tree} (process-dir-partition cmds)
        free-space (- 70000000 total-size)
        needed-space (- 30000000 free-space)
        candidates (find-dirs-greater-than-size tree needed-space)]
    (apply min candidates)))

(def solution
  {:year 2022
   :day 7
   :part-1 part-1
   :part-2 part-2})

(comment
  (require '[aoc-clj.core :as aoc])
  (aoc/run-solution solution))
