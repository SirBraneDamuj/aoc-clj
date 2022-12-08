(ns aoc-clj.aoc-2022.day-07-test
  (:require [aoc-clj.aoc-2022.day-07 :as day-07]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]))

(def test-input
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

(def real-input
  (slurp (io/file "resources" "real-input" "2022" "day07.txt")))

(deftest part-1
  (testing "satisfies test input"
    (is (= 95437 (day-07/part-1 test-input))))
  (testing "satisfies real input"
    (is (= 1667443 (day-07/part-1 real-input)))))

(deftest part-2
  (testing "satisfies test input"
    (is (= 24933642 (day-07/part-2 test-input))))
  (testing "satisfies real input"
    (is (= 8998590 (day-07/part-2 real-input)))))
