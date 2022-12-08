(ns aoc-clj.utils
  (:require [clojure.string :as str]))

(defn parse-binary
  [s]
  (Integer/parseInt s 2))

(defn split-newline-delim-line-groups
  [s]
  (str/split s #"(\n\n)|(\r\n\r\n)"))
