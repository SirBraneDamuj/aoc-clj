(ns aoc-clj.utils
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))


(defn ppeek
  [x]
  (pprint x)
  x)

(defn parse-binary
  [s]
  (Integer/parseInt s 2))

(defn split-newline-delim-line-groups
  [s]
  (str/split s #"(\n\n)|(\r\n\r\n)"))
